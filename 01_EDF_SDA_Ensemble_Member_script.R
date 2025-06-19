### Code for EDF uncertainty analyses ###
# charlotte's version

# set working directory
setwd("/projectnb/dietzelab/malmborg/EDF/")

#install.packages("DBI",repos = "https://cloud.r-project.org")
#install.packages("sf",repos = "https://cloud.r-project.org", type = "source", configure.args = "--with-gdal-config=/share/pkg.8/gdal/3.8.4/install/bin/gdal-config")
library(sf)
library(terra)

## Loading SDA ensemble data
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/"
# get directories that include tiff files:
dirs <- list.dirs(paste0(ens, run))[-grep("07-15", list.dirs(paste0(ens, run)))]
# choose variable:
var <- "TotSoilCarb_"
# load list of files:
varfiles <- dirs[grep(var, dirs)]

# load first example - 6/10 testing with one tiff, can be put into loop
# choose year:
year <- 2021
# choose file by year:
findfile <- paste0(ens, run, var, as.character(year))
findtiff <- list.files(findfile)[grep(".tiff", list.files(findfile))]
# getting specific tiff:
tiff <- paste0(findfile, "/", findtiff[1])
soc <- terra::rast(tiff)
#terra::plot(soc)

# load LandIQ:
v <- terra::vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")
# match projection with tiff:
v <- terra::project(v, soc)

## Crop SDA to LandIQ
# crop California:
socCA <- terra::crop(soc, v)
#terra::plot(socCA)
#maps::map("state", add = T)

# align LandIQ to SDA:
landClass = read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                       header=TRUE,sep="\t")

v["CF"] = as.factor(v["MAIN_CROP"])
landRast <- terra::rasterize(v, socCA, "MAIN_CROP")
#terra::plot(landRast)
#maps::map("state", add = TRUE)

## code to reclassify land classes
fromClass <- paste0(landClass$CLASS,landClass$SUBCLASS)
fromClass <- sub("NA","",fromClass)
reClass <- data.frame(from = fromClass, 
                      to = landClass$CLASS)
reClass$ref <- as.numeric(as.factor(reClass$to))  # reference for making numeric to do classify
classed = terra::classify(landRast, reClass$ref)  # this one requires numeric not character entry for classify
#terra::plot(classed)
#maps::map("state",add=TRUE)


## Aggregate by crop type
landDF <- as.data.frame(landRast) %>% tibble::rownames_to_column()
socDF  <- as.data.frame(socCA) %>% tibble::rownames_to_column()
# change lyr.1 column name if single emsemble member and not ensemble mean tiff:
if ("lyr.1" %in% colnames(socDF) == TRUE){
  socDF <- socDF %>% dplyr::rename("mean" = "lyr.1")
}
join <- dplyr::inner_join(landDF, socDF, by = "rowname")
join <- dplyr::inner_join(join, reClass, by = dplyr::join_by("MAIN_CROP" == "from"))

# tapply(join$mean, join$MAIN_CROP, mean, na.rm=TRUE)  # for non-mean ensembles the column to site is "lyr.1"
# tapply(join$mean, join$MAIN_CROP, sum, na.rm=TRUE)
# 
# tapply(join$mean, join$to, mean, na.rm=TRUE)
# tapply(join$mean, join$to, sum, na.rm=TRUE)


## Calculate by county for just cropland
CoORIG = terra::vect("/projectnb/dietzelab/dietze/CARB/CA_Counties.shp")
Co <- terra::project(CoORIG, socCA)
#terra::plot(Co)
CoSOC <- terra::extract(socCA, Co, fun = mean, na.rm = TRUE)
# change lyr.1 column name if single emsemble member and not ensemble mean tiff:
if ("lyr.1" %in% colnames(CoSOC) == TRUE){
  CoSOC <- CoSOC %>% dplyr::rename("mean" = "lyr.1")
}
Co[["soc"]] <- CoSOC$mean
#terra::plot(Co, "soc")

isCrop = !is.na(landRast)
terra::plot(isCrop)
maps::map("state",add=TRUE)

cropSOC = socCA*isCrop
CoCropSOC <- terra::extract(cropSOC, Co, fun = mean, na.rm = TRUE)
# change lyr.1 column name if single emsemble member and not ensemble mean tiff:
if ("lyr.1" %in% colnames(CoCropSOC) == TRUE){
  CoCropSOC <- CoCropSOC %>% dplyr::rename("mean" = "lyr.1")
}
Co[["cropSOC"]] <- CoCropSOC$mean
terra::plot(Co, "cropSOC", legend = "topright")

CoCropTotSOC <- terra::extract(cropSOC*100/1000000, Co, fun = sum, na.rm = TRUE)
# change lyr.1 column name if single emsemble member and not ensemble mean tiff:
if ("lyr.1" %in% colnames(CoCropTotSOC) == TRUE){
  CoCropTotSOC <- CoCropTotSOC %>% dplyr::rename("sum" = "lyr.1")
}
Co[["cropTotSOC"]] <- CoCropTotSOC$sum
terra::plot(Co, "cropTotSOC", legend = "topright")

# CoCropSOCsd <- terra::extract(cropSOC, Co, fun = sd, na.rm = TRUE)
# # change lyr.1 column name if single emsemble member and not ensemble mean tiff:
# if ("lyr.1" %in% colnames(CoCropSOCsd) == TRUE){
#   CoCropSOCsd <- CoCropSOCsd %>% dplyr::rename("sd" = "lyr.1")
# }
# Co[["cropSOCsd"]] <- CoCropSOCsd$sd
# terra::plot(Co, "cropSOCsd", legend = "topright")


palatte = c('#edf8e9','#bae4b3','#74c476','#31a354','#006d2c')
terra::plot(Co, "cropSOC", legend = "topright", col = palatte)
terra::plot(Co, "cropTotSOC", legend = "topright", col = palatte)

## NAIVE uncertainty calculation: Sum independent Var
# * load SD map
# * calc SD*SD*isCrop
# * extract
# * add sqrt to df
# for SDA ensemble members' sd files:
sdtiff <- paste0(findfile, "/", findtiff[grep("std", findtiff)])
socSD <- terra::rast(sdtiff)
socSDCA <- terra::crop(socSD, v)
cropVar <- isCrop*socSDCA^2
CoCropTotVar = terra::extract(cropVar, Co, fun = sum, na.rm = TRUE)
Co[["crop_TotSOC_CV"]] <- sqrt(CoCropTotVar$MAIN_CROP)*100/1000000/CoCropTotSOC$sum*100
Co[["crop_TotSOC_SD"]] <- sqrt(CoCropTotVar$MAIN_CROP)*100/1000

terra::plot(socSDCA, legend = "topright")
maps::map("state", add = TRUE)
terra::plot(Co, "crop_TotSOC_CV", legend = "topright")
terra::plot(Co, "crop_TotSOC_SD", legend = "topright")

## ENSEMBLE uncertainty calculation:
 # Calc sum for different ensembles, then calc SD
 #  * load each ensemble
 #  * clip to CA
 #  * multiply by isCrop
 #  * extract
 #  * add to df
 #  * calc SD over df
 #  * add to Co map
ne = 25
ensAGB = as.data.frame(matrix(NA, nrow = 58, ncol = ne))
for (e in 1:ne) {
  print(e)
  ## load & clip ensemble member
  fname = paste0("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/downscale_maps/AbvGrndWood_2021/ensemble_",
                 e,"_2021_AbvGrndWood.tiff")
  eagb <- terra::crop(terra::rast(fname),v) * isCrop
  ensTotCropAGB = terra::extract(eagb,Co,fun=sum,na.rm=TRUE)
  ensAGB[,e] = ensTotCropAGB[,2]*100/1000000 ## Mg/ha -> Tg
}
Co[["crop_ensAGB_SD"]] <- apply(ensAGB,1,sd)*1000 #Gg 

palatte = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
breaks = c(0,10^seq(0,log10(1000),length=9))
terra::plot(Co,"crop_ensAGB_SD",legend="topright",breaks=breaks,col=palatte)
terra::plot(Co,"crop_TotAGB_SD",legend="topright",breaks=breaks,col=palatte)
#range=c(0,500),


### making a function to change "lyr.1" to "mean" for column names repeatedly:
#'@param df = dataframe object
#'@param oldname = Character: old column name to be changed
#'@param newname = Character: new column name
switch_col_name <- function(df, oldname, newname){
  # change lyr.1 column name if single emsemble member and not ensemble mean tiff:
  if ("lyr.1" %in% colnames(df) == TRUE){
    df <- df %>% dplyr::rename(newname = oldname)
    return(df)
  }
}

test <- data.frame(x = c(1:5), 
                   lyr.1 = c(10:14))
out <- switch_col_name(test, "lyr.1", "new")

#* Repeat for SoilC







# read gedi mean value from the geotiff file.
gedi <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GEDI/GEDI04_B_MW019MW223_02_002_02_R01000M_MU.tif")
# read the base map for referencing resolution, extents, and coordinate reference system (crs).
base.map <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/downscale/MODIS_NLCD_LC.tif")
# read landtrendr AGB image from 2017.
landtrendr.agb <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GEDI/agb_obs_mean_resample_2017_30m.tif")
# aggregate landtrendr AGB into 1 km resolution for comparison.
landtrendr.agb.1km <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GEDI/agb_obs_mean_resample_2017_1km.tif")
# reproject gedi into the same crs as the base map.
gedi <- terra::project(gedi, base.map)
# plot gedi map to make sure everything is good.
terra::plot(gedi)
# convert pecan settings into geospatial points.
# pts <- pecan.settings.2.pts("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA/pecanIC.xml")
pts <- terra::vect("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/AGU_2024/pts.shp")
# extract gedi mean value for the points.
gedi.pts <- terra::extract(gedi, pts)

# load analysis results from the SDA.
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA/sda.all.forecast.analysis.Rdata")
# define t = 9, which is the year 2020.
t <- 9
analysis.t <- analysis.all[[t]]
analysis.t.mean <- colMeans(analysis.t)

# calculate the ensemble means for the AGB results from the SDA.
agb.inds <- seq(1, 25600, 4)
# agb.analysis.pts <- sda.outputs[["enkf.params"]][["mu.a"]][agb.inds]
agb.analysis.pts <- analysis.t.mean[agb.inds]

# store the SDA analysis into the data frame.
gedi.pts$analysis <- agb.analysis.pts

# landtrendr part.
# extract landtrendr from both 30m and 1km resolutions into points.
landtrendr.agb.pts <- terra::extract(landtrendr.agb, pts)
landtrendr.agb.1km.pts <- terra::extract(landtrendr.agb.1km, pts)

# add both extracted landtrendr values into the data frame.
gedi.pts$landtrendr <- landtrendr.agb.pts[,2]
gedi.pts$landtrendr.1km <- landtrendr.agb.1km.pts[,2]

# compare both CONUS and south NA points to GEDI.
# grab point index that are just within the CONUS US.
CONUS.NA.inds <- which(!is.na(gedi.pts$landtrendr) & !is.na(gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU))
# grab point index that are just within the southern NA.
south.NA.inds <- which(!is.na(gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU) & is.na(gedi.pts$landtrendr))
# grab point index that are within GEDI domain.
# gedi.NA.inds <- which(!is.na())

# plot GEDI vs SDA analysis results for just CONUS.
plot(gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU[CONUS.NA.inds], gedi.pts$analysis[CONUS.NA.inds], pch=18)
# plot GEDI vs SDA analysis results for just southern NA.
points(gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU[south.NA.inds], gedi.pts$analysis[south.NA.inds], pch=8, col=2)
abline(0,1,lwd=2,col=3)
# calculate the correlation between GEDI and SDA under the previous two situations.
cor(gedi.pts$analysis[CONUS.NA.inds], gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU[CONUS.NA.inds], use="complete.obs")
cor(gedi.pts$analysis[south.NA.inds], gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU[south.NA.inds], use="complete.obs")

plot(gedi.pts$GEDI04_B_MW019MW223_02_002_02_R01000M_MU, gedi.pts$landtrendr.1km, pch=18)
