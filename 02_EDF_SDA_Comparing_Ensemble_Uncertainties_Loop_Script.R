#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN CALIFORNIA ####

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
library(dplyr)
# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/"
# # get directories that include tiff files:
# dirs <- list.dirs(paste0(ens, run))[-grep("07-15", list.dirs(paste0(ens, run)))]

# C variables:
agb <- "AbvGrndWood_"
lai <- "LAI_"
smf <- "SoilMoistFrac_"
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- 2021
dir <- paste0(ens, run)

# load information for region and aggregation scale:
region <- "/projectnb/dietzelab/dietze/CARB/CA_Counties.shp"
crops <- "/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp"
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")

# information for saving maps and output: still need to include
#map_dir <- ""

## Function for processing each tif file
# inputs:
#'@param dir = Character vector: file where tiffs can be found
#'@param var = variable of interest for each analysis; e.g. soc
#'@param year = numeric: data year of ensemble members
#'@param crops = vector: shapefile for croplands to crop rasters
process_ensemble_members <- function(dir, var, year, crops){
  # choose file by year:
  findfile <- paste0(dir, var, as.character(year))
  findtiff <- list.files(findfile)[grep(".tiff", list.files(findfile))]
  
  # load vector for region:
  vec <- terra::vect(crops)
  
  # loop for processing and making list of rasters:
  ens_rast <- list()
  for (i in 1:length(findtiff)){
    print(i)
    tiff <- paste0(findfile, "/", findtiff[i])
    rast <- terra::rast(tiff)
    vec <- terra::project(vec, rast)
    crop <- terra::crop(rast, vec)
    ens_rast[[findtiff[i]]] <- crop
  }
  #ens_rast[["full_map"]] <- terra::rast(paste0(findfile, "/", findtiff[grep("mean", findtiff)]))
  return(ens_rast)
}


## Function for identifying crop regions:
# inputs:
#'@param rast = example raster for making object for sorting where crops are/aren't
#'@param crops = vector shapefile of croplands
# can be added later when sorting by crop types:
#'@param classes = table of crop classes for identifying croplands
get_crop <- function(rast, crops){
  # load crops as vector and reproject:
  cv <- terra::vect(crops)
  cv <- terra::project(cv, rast)
  # crop example raster to croplands:
  crop_rast <- terra::crop(rast, cv) 
  # load crop classes
  #landClass <- classes
  #cv["CF"] = as.factor(cv["MAIN_CROP"])  # this line throws an error that doesn't prevent the code from running but does seem to stop the function
  landRast <- terra::rasterize(cv, crop_rast, "MAIN_CROP")
  is_crop <- !is.na(landRast)
  return(is_crop)
}


## Function for running analyses ##
# inputs for running analyses in chosen location and scale:
#'@param ens_rast List: list of tiff files from processing function
#'@param is_crop Raster: cropland selection raster from is_crop function
#'@param region shapefile: vector shapefile for aggregation region, e.g. county
naive_C_uncertainty <- function(ens_rast, is_crop, region){
  # separate mean/std tiffs and ensemble members:
  ens_mean <- ens_rast[[names(ens_rast)[grep("mean", names(ens_rast))]]]
  ens_std <- ens_rast[[names(ens_rast)[grep("std", names(ens_rast))]]]
  
  # Calculate by county:
  RegORIG <- terra::vect(region)
  Reg <- terra::project(RegORIG, ens_mean)
  RegVar <- terra::extract(ens_mean, Reg, fun = mean, na.rm = TRUE)
  Reg[["mean"]] <- RegVar$mean
  
  # means for area that is just cropland:
  cropMean <- ens_mean*is_crop
  RegCropMean <- terra::extract(cropMean, Reg, fun = mean, na.rm = TRUE)
  Reg[["cropVarMean"]] <- RegCropMean$mean
  
  # totals for area that is just cropland:
  RegCropTotMean <- terra::extract(cropMean*100/1000000, Reg, fun=sum, na.rm = TRUE) #Mg/ha -> Tg
  Reg[["cropTotVar"]] <- RegCropTotMean$mean

  # naive uncertainty:
  cropVar <- is_crop*ens_std^2
  RegCropTotVar = terra::extract(cropVar, Reg, fun = sum, na.rm = TRUE)
  Reg[["crop_Tot_CV"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000000/RegCropTotMean$mean*100
  Reg[["crop_Tot_SD"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000
  
  # return data for making maps:
  return(Reg)
}


## Function for ensemble uncertainty calculations:
#'@param ens_rast List: list of raster objects from processing function
#'@param n_regions numeric: number of divisions in regions for aggregating, e.g. number of counties
#'@param is_crop Raster: cropland selection raster from is_crop function
#'@param Reg region variable from previous function
ensemble_C_uncertainty <- function(ens_rast, n_regions, Reg){
  # separate ensemble members from mean and std rasters:
  ensems <- ens_rast[names(ens_rast)[grep("ensemble", names(ens_rast))]]
  
  # loop for extracting ensemble member sums:
  ne = length(ensems)
  ens_mems <- as.data.frame(matrix(NA, nrow = n_regions, ncol = ne))
  
  # Calculate sum for different ensembles, then calculate SD
  ne = 25
  ensVar = as.data.frame(matrix(NA, nrow = 58, ncol = ne))
  for (e in 1:ne) {
    print(e)
    # load ensemble member:
    tiff <- ens_rast[[e]]
    # clip the ensemble member for crops:
    ens_var <- tiff * is_crop
    # extract for region of aggregation:
    ensTotCropVar <- terra::extract(ens_var, Reg, fun = sum, na.rm = TRUE)
    # add to data frame + Tg conversion:
    ensVar[,e] <- ensTotCropVar[,2]*100/1000000 ## Mg/ha -> Tg
  }
  # calculate sd over dataframe
  Reg[["crop_ensVar_SD"]] <- apply(ensVar, 1, sd)*1000 #Gg 
  return(Reg)
}

# number of aggregate regions (e.g. counties): this runs for California, would need to be fixed for each place
agg_reg <- vect(region)
n_regions <- length(unique(agg_reg$NAME))

# test functions:
ens_rast <- process_ensemble_members(dir, soc, 2021, crops)
is_crop <- get_crop(ens_rast[[1]], crops)
Reg <- naive_C_uncertainty(ens_rast, is_crop, region)
Reg <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)  # still need to find a good way to deal with number of counties/aggregate regions

# # test with different variable and year:
# ens_rast <- process_ensemble_members(dir, agb, 2022, crops)
# is_crop <- get_crop(ens_rast[[1]], crops)
# Reg_agb <- naive_C_uncertainty(ens_rast, is_crop, region)
# Reg_agb <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)

# when I have to make plots
terra::plot(Reg,"cropVarMean",legend="topright")
terra::plot(Reg, "crop_Tot_SD", legend = "topright")
terra::plot(Reg, "crop_ensVar_SD", legend = "topright")

# terra::plot(Reg_agb, "crop_Tot_SD", legend = "topright")
# terra::plot(Reg_agb, "crop_ensVar_SD", legend = "topright")


## SD vs aggregation area plots



### ARCHIVE ###
r <- terra::vect(region)
r <- project(r, ens_rast[[1]])
r$area_m2 <- terra::expanse(r, unit = "m")
countyRast <- rasterize(r, is_crop, field = "NAME")
countyTable <- freq(countyRast)
rast_poly <- as.polygons(is_crop)

county_sort <- as.data.frame(r) %>%
  arrange(NAME)

reclass_iscrop <- as.factor(is_crop)
#reclass_iscrop <- classify(reclass_iscrop, rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))
reclass_iscrop <- ifel(is_crop == 1, 1, NA)
rast_poly <- as.polygons(reclass_iscrop)
test_crop <- terra::intersect(r, rast_poly)
#zonal_test <- terra::zonal(rast_poly, r, fun = "sum")

overlap <- terra::intersect(r, rast_poly)
overlap$area_m2 <- terra::expanse(overlap, unit="m")
agg_areas <- as.data.frame(overlap) %>%
  group_by(NAME) %>%              
  summarise(overlap_area_m2 = sum(area_m2, na.rm=TRUE))

# # Create a mask raster: keep 1s, set others to NA
# r_masked <- classify(r, cbind(0, NA))  # keeps 1s, 0 → NA
# 
# # Calculate cell area raster (in m²)
# cell_areas <- cellSize(r, unit="m")
# 
# # Multiply masked raster by cell area to get per-cell area where condition is true
# area_raster <- r_masked * cell_areas
# 
# # Use zonal() to sum area in m² by county
# area_by_county <- zonal(area_raster, zones, fun="sum", na.rm=TRUE)
# 
# # area_by_county is a data.frame with county ID and total area in m²
# print(area_by_county)


# # reclassify land classes:
# fromClass <- paste0(landClass$CLASS,landClass$SUBCLASS)
# fromClass <- sub("NA","",fromClass)
# reClass <- data.frame(from = fromClass, 
#                       to = landClass$CLASS)
# reClass$ref <- as.numeric(as.factor(reClass$to))  # reference for making numeric to do classify
# classed <- terra::classify(landRast, reClass$ref)  # this one requires numeric not character entry for classify
# 
# # Aggregate by crop type:
# landDF <- as.data.frame(landRast) %>% tibble::rownames_to_column()
# cropDF  <- as.data.frame(crop_mean) %>% tibble::rownames_to_column()
# join <- dplyr::inner_join(landDF, cropDF, by = "rowname")
# join <- dplyr::inner_join(join, reClass, by = dplyr::join_by("MAIN_CROP" == "from"))


### original loading files code:
# # choose file by year:
# findfile <- paste0(ens, run, var, as.character(year))
# findtiff <- list.files(findfile)[grep(".tiff", list.files(findfile))]
# # identify where to find those files:
# varfiles <- dirs[grep(var, dirs)]
# # getting specific tiffs for the ensemble means and std devs:
# yr_mean <- paste0(findfile, "/", findtiff[grep("mean", findtiff)])
# yr_std <- paste0(findfile, "/", findtiff[grep("std", findtiff)])


### original function code:
# inputs for running analyses in chosen location and scale:
#'@param yr_mean = raster: mean 25-ensemble member tiff file
#'@param yr_std = raster: std 25-ensemble member tiff file
#'@param crop = vector: cropland shapefile
#'@param reg = vector: aggregation scale region, e.g. county
#'@param classes = dataframe: table of crop classes for aligning to SDA
# compare_C_uncertainty <- function(yr_mean,
#                                   yr_std,
#                                   crops,
#                                   reg,
#                                   classes){
#   # load tiff (raster) files for specified variable and year:
#   yr_mean <- terra::rast(yr_mean)
#   yr_std <- terra::rast(yr_std)
#   # load vector file for crop and reproject to match rasters:
#   cv <- terra::vect(crops)
#   cv <- terra::project(cv, yr_mean)
#   # load vector for aggregate region and reproject to match rasters:
#   v_reg <- terra::vect(reg)
#   v_reg <- terra::project(v_reg, yr_mean)
#   
#   # crop variable rasters to crop lands:
#   crop_mean <- terra::crop(yr_mean, cv) 
#   crop_std <- terra::crop(yr_std, cv)
#   
#   # match crops to crop classes
#   landClass <- classes
#   cv["CF"] = as.factor(cv["MAIN_CROP"])
#   landRast <- terra::rasterize(cv, crop_mean, "MAIN_CROP")
#   
#   # Aggregate by region mean and total:
#   RegCrop <- terra::extract(crop_mean, v_reg, fun = mean, na.rm = TRUE)
#   RegCrop[["mean"]] <- RegCrop$mean
#   # aggregation based on cropland in region:
#   isCrop <- !is.na(landRast)
#   cropMean <- crop_mean*isCrop
#   # mean of selected SDA variable:
#   RegCropMean <- terra::extract(cropMean, v_reg, fun = mean, na.rm = TRUE)
#   v_reg[["cropMean"]] <- RegCropMean$mean
#   # total of selected SDA variable in each region grouping:
#   RegCropTot <- terra::extract(cropMean*100/1000000, v_reg, fun = sum, na.rm = TRUE)
#   v_reg[["cropTot"]] <- RegCropTot$mean
#   
#   # naive uncertainty:
#   cropVar <- isCrop*crop_std^2
#   RegCropTotVar = terra::extract(cropVar, v_reg, fun = sum, na.rm = TRUE)
#   v_reg[["crop_Tot_CV"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000000/RegCropTot$mean*100
#   v_reg[["crop_Tot_SD"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000
  

# # dealing with county areas:
# test_crop <- intersect(rast_poly, r)
# # there is already a shape_area function lol
# terra::zonal(is_crop, r, fun = "sum")  # I think this is pixel numbers?
# #zonal_test <- terra::zonal(test_crop, r, fun = "sum") # this doesn't look right to me when I plot
# 
# cell_area <- cellSize(is_crop)
# 
# r_masked <- mask(is_crop, r)
# area_masked <- mask(cell_areas, v)
# 
# rastPoints <- as.points(is_crop)
# rastCounties <- extract(r, rastPoints)