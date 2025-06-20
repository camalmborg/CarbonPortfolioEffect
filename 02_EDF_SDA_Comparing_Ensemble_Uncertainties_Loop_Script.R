#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN CALIFORNIA ####

## Prepare workspace ##
# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

# load libraries:
library(sf)
library(terra)
library(dplyr)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/"
# get directories that include tiff files:
dirs <- list.dirs(paste0(ens, run))[-grep("07-15", list.dirs(paste0(ens, run)))]

# C variables:
agb <- "AbvGrndWood_"
lai <- "LAI_"
smf <- "SoilMoistFrac_"
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- 2021
# choose file by year:
findfile <- paste0(ens, run, var, as.character(year))
findtiff <- list.files(findfile)[grep(".tiff", list.files(findfile))]
# identify where to find those files:
varfiles <- dirs[grep(var, dirs)]
# getting specific tiffs for the ensemble means and std devs:
yr_mean <- paste0(findfile, "/", findtiff[grep("mean", findtiff)])
yr_std <- paste0(findfile, "/", findtiff[grep("std", findtiff)])

# load information for region and aggregation scale:
crops <- "/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp"
reg <- "/projectnb/dietzelab/dietze/CARB/CA_Counties.shp"
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")

# information for saving maps and output:
map_dir <- ""


## Function for running analyses ##
# inputs for running analyses in chosen location and scale:
#'@param yr_mean = raster: mean 25-ensemble member tiff file
#'@param yr_std = raster: std 25-ensemble member tiff file
#'@param crop = vector: cropland shapefile
#'@param reg = vector: aggregation scale region, e.g. county
#'@param classes = dataframe: table of crop classes for aligning to SDA
compare_C_uncertainty <- function(yr_mean,
                                  yr_std,
                                  crops,
                                  reg,
                                  classes){
  # load tiff (raster) files for specified variable and year:
  yr_mean <- terra::rast(yr_mean)
  yr_std <- terra::rast(yr_std)
  # load vector file for crop and reproject to match rasters:
  cv <- terra::vect(crops)
  cv <- terra::project(cv, yr_mean)
  # load vector for aggregate region and reproject to match rasters:
  r <- terra::vect(reg)
  r <- terra::project(r, yr_mean)
  
  # crop variable rasters to crop lands:
  crop_mean <- terra::crop(yr_mean, cv) 
  crop_std <- terra::crop(yr_std, cv)
  
  # match crops to crop classes
  landClass <- classes
  cv["CF"] = as.factor(cv["MAIN_CROP"])
  landRast <- terra::rasterize(cv, crop_mean, "MAIN_CROP")
  
  # Aggregate by region mean and total:
  RegCrop <- terra::extract(crop_mean, r, fun = mean, na.rm = TRUE)
  RegCrop[["mean"]] <- RegCrop$mean
  # aggregation based on cropland in region:
  isCrop <- !is.na(landRast)
  cropMean <- crop_mean*isCrop
  # mean of selected SDA variable:
  RegCropMean <- terra::extract(cropMean, r, fun = mean, na.rm = TRUE)
  r[["cropMean"]] <- RegCropMean$mean
  # total of selected SDA variable in each region grouping:
  RegCropTot <- terra::extract(cropMean*100/1000000, r, fun = sum, na.rm = TRUE)
  r[["cropTot"]] <- RegCropTot$mean
  
  # naive uncertainty:
  cropVar <- isCrop*crop_std^2
  RegCropTotVar = terra::extract(cropVar, r, fun = sum, na.rm = TRUE)
  r[["crop_Tot_CV"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000000/RegCropTot$mean*100
  r[["crop_Tot_SD"]] <- sqrt(RegCropTotVar$MAIN_CROP)*100/1000
  
  # ensemble uncertainty:
  ne = 25
  ensAGB = as.data.frame(matrix(NA, nrow = 58, ncol = ne)) # 58 is probably the number of counties in California
  for (e in 1:ne) {
    print(e)
    ## load & clip ensemble member
    fname = paste0("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/downscale_maps/AbvGrndWood_2021/ensemble_",
                   e,"_2021_AbvGrndWood.tiff")
    eagb <- terra::crop(terra::rast(fname),v) * isCrop
    ensTotCropAGB = terra::extract(eagb,Co,fun=sum,na.rm=TRUE)
    ensAGB[,e] = ensTotCropAGB[,2]*100/1000000 ## Mg/ha -> Tg
  }
  Co[["crop_ensAGB_SD"]] <- apply(ensAGB, 1, sd)*1000 #Gg 
}


### ARCHIVE ###

countyRast <- rasterize(r, isCrop, field = "NAME")
countyTable <- freq(countyRast)
rast_poly <- as.polygons(isCrop)
test_crop <- intersect(rast_poly, r)
# there is already a shape_area function lol
terra::zonal(isCrop, r, fun = "sum")  # I think this is pixel numbers?
zonal_test <- terra::zonal(test_crop, r, fun = "sum") # this doesn't look right to me when I plot

cell_area <- cellSize(isCrop)

r_masked <- mask(isCrop, r)
area_masked <- mask(cell_areas, v)

rastPoints <- as.points(isCrop)
rastCounties <- extract(r, rastPoints)

reclass_iscrop <- as.factor(isCrop)
#reclass_iscrop <- classify(reclass_iscrop, rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))
reclass_iscrop <- ifel(isCrop == 1, 1, NA)
rast_poly <- as.polygons(reclass_iscrop)
test_crop <- intersect(r, rast_poly)
#zonal_test <- terra::zonal(rast_poly, r, fun = "sum")

overlap <- terra::intersect(r, rast_poly)
overlap$area_m2 <- terra::expanse(overlap, unit="m")
overlap <- as.data.frame(overlap) %>%
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