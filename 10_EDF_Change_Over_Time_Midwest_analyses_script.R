### Script for running change over time analyses ###

## Load libraries
library(dplyr)
library(terra)
library(sf)

## Load functions and data
# functions for portfolio processing and analysis:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Portfolio_Sampling_functions_script.R")

# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Load Data:
# navigate to change over time maps:
cot <- "Change_Over_Time/"
# years of run:
yr_one = 2019
yr_two = 2024

# load raster cell sizes:
cell_size <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/cell_size.tif")
# crop types layer:
crop_types_layer <- "rasters/2021_30m_cdls/2021_30m_cdls.tif"
# get Midwest region for cropping crop types raster:
# tigris states:
cornstates_sf <- tigris::states(year = yr_two) %>%
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
crop_types_mask <- mask(crop_types_crop, cornregion)

## Attempt to make into vector object:
# aggregate to 1km:
crop_types_agg <- aggregate(crop_types_mask, fact = 1000/30, fun = "modal")
# resample to get to 1km:
one_k <- 1000 # 1000 x 1000 m
temp <- rast(round(ext(crop_types_agg), 0), resolution = one_k, crs = crs(crop_types_agg))
crop_types_resamp <- resample(crop_types_agg, temp, method = "near")
# make into polygons:
crop_types_vec <- as.polygons(crop_types_resamp)
crop_types_vec <- disagg(crop_types_vec)
# clean up (not super necessary):
rm(crop_types_rast, crop_types_crop, crop_types_agg, temp, one_k)

## Process NA SDA product for MW:
# C variables:
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- yr_two
dir <- cot
ens_rast <- process_ensemble_members(dir = dir,
                                     var = var,
                                     year = yr_two,
                                     crops = crop_types_vec,
                                     cell_size = cell_size)

## Make polygons of particular crop classes
crop_types_vec <- project(crop_types_vec, ens_rast[[1]])
# convert terra vector to sf for easier separation:
crops_sf <- st_as_sf(crop_types_vec) 
# separate some groups:
# corn crops:
corn <- crops_sf |>
  filter(Class_Names == "Corn")
# soybeans:
soybeans <- crops_sf |>
  filter(Class_Names == "Soybeans")
# Grass/Pasture:
grass_pasture <- crops_sf |>
  filter(Class_Names == "Grassland/Pasture")

# add to a list:
crop_group_list <- list(corn = corn, 
                        soybeans = soybeans, 
                        grass_pasture = grass_pasture)

