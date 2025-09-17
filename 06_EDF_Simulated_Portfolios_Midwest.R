### Simulated portfolios ###
# in Midwest

## Load Libraries
library(sf)
library(terra)
library(dplyr)
library(tigris)

# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

# source processing functions:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")

## Load Data:
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"
# crop types layer:
crop_types_layer <- "rasters/2021_30m_cdls/2021_30m_cdls.tif"

# get Midwest region for cropping crop types raster:
# tigris states:
cornstates_sf <- tigris::states() %>%
  filter(NAME %in% c("Illinois", "Indiana", "Iowa"))
cornstates <- vect(cornstates_sf)
rm(cornstates_sf)
# aggregate and dissolve to get outline of states:
cornregion <- aggregate(cornstates, dissolve = TRUE)

## Make vector crop types object from raster layer
crop_types_rast <- terra::rast(crop_types_layer)
# crop to vector area:
cornregion <- project(cornregion, crop_types_rast)
crop_types_crop <- crop(crop_types_rast, cornregion)
# save it so I don't have to deal with the projection time again:
#writeRaster(crop_types_crop, filename = "rasters/crop_types_rast_MW_crop.tif")

# aggregate to 1km:
crop_types_agg <- aggregate(crop_types_crop, fact = 1000/30, fun = "modal")
# resample to get to 1km:
one_k <- 1000 # 1000 x 1000 m 
temp <- rast(round(ext(crop_types_agg), 0), resolution = one_k, crs = crs(crop_types_agg))
crop_types_resamp <- resample(crop_types_agg, temp, method = "near")
# clean up (not super necessary):
rm(crop_types_rast, crop_types_crop, crop_types_agg, temp)

