#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN THE MIDWEST ####
# note: specifically for Illinois, Indiana, Iowa

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
library(dplyr)
library(tidyverse)
library(tigris)

# source functions:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/02_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")

# set directory
dir <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(dir)

# load midwestern croplands from USGS 30m croplands extent layer:
rast_crops <- rast("rasters/midwest_croplands.tif")
rast_crops <- project(rast_crops, "EPSG:3857")

# testing tigris:
cornbelt <- vect(tigris::counties(c("Illinois", "Indiana", "Iowa"))) # Note FIPS codes for states: 17 = Illinois, 18 = Indiana, 19 = Iowa
# reproject:
cornbelt <- terra::project(cornbelt, rast_crops)

# crop the raster to study area:
rast_crops <- terra::crop(rast_crops, cornbelt)
# apply mask:
rast_crops_mask <- terra::mask(rast_crops, cornbelt)
# make a croplands area raster:
rast_crops_mask[rast_crops_mask != 2] <- NA
rast_crops_mask[rast_crops_mask == 2] <- 1
# save for later:
writeRaster(rast_crops_mask, "rasters/midwest_crops_classed.tif")

# test is_crop function:
test <- get_is_crop(rast_crops_mask)

# ### Merging croplans raster tiles for Midwest region
# # load raster for crops:
# # note: 2 = cropland areas
# rasters <- list.files(paste0(dir, "rasters/"))
# # make list of rasters:
# rast_list <- list()
# for (i in 1:length(rasters)){
#   rast_list[i] <- rast(paste0(dir, "/rasters/", rasters[i]))
# }
# # make a raster collection for merge:
# rast_collection <- sprc(rast_list)
# # Merge raster tiles:
# rast_merge <- merge(rast_collection)
# # save it
# #writeRaster(rast_merge, "rasters/midwest_croplands.tif")
# rm(rast, rast_collection, rast_list, rast_merge)

# ### Alternative to tigris county vectors
# # loading vector of counties:
# us_counties <- terra::vect("shapefiles/tl_2024_us_county.shp")
# # reproject to match croplands raster:
# us_counties <- terra::project(us_counties, rast_crops)
# # separate counties for midwestern states Illinois, Indiana, Iowa:
# cornbelt <- us_counties[us_counties$STATEFP %in% c("17", "18", "19")]