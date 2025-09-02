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
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")

# load raster cell sizes:
cell_size <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/cell_size.tif")

# set directory
dir <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(dir)

# # load midwestern croplands from USGS 30m croplands extent layer:
rast_crops <- rast("rasters/midwest_croplands.tif")
rast_crops <- project(rast_crops, "EPSG:3857")

# tigris counties:
cornbelt <- vect(tigris::counties(c("Illinois", "Indiana", "Iowa"))) # Note FIPS codes for states: 17 = Illinois, 18 = Indiana, 19 = Iowa
# reproject:
cornbelt <- terra::project(cornbelt, rast_crops)

# tigris townships (towns, villages, etc):
corntowns <- vect(tigris::places(state = c("Illinois", "Indiana", "Iowa"), cb = FALSE))
corntowns <- terra::project(corntowns, rast_crops)

# tigris states:
cornstates_sf <- tigris::states() %>%
  filter(NAME %in% c("Illinois", "Indiana", "Iowa"))
cornstates <- vect(cornstates_sf)
rm(cornstates_sf)
cornstates <- terra::project(cornstates, rast_crops)

# aggregate and dissolve to get outline of states:
cornregion <- aggregate(cornstates, dissolve = TRUE)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
#run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/"
#run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_GEDI_rf/"
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"

# test functions on Midwest counties
## Preparing for running selected variable and year across aggregate regions:
# C variables:
agb <- "AbvGrndWood_"
lai <- "LAI_"
smf <- "SoilMoistFrac_"
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- 2021
dir <- paste0(ens, run)

## Areas for calculating carbon uncertainty
# town group:
agg_twn <- corntowns
n_towns <- length(unique(corntowns$GEOID))
# county group:
agg_counties <- cornbelt
n_counties <- length(unique(cornbelt$GEOID))
# state group:
agg_states <- cornstates
n_states <- length(unique(cornstates$GEOID))
# region group:
agg_region <- cornregion
n_reg <- 1


# croplands:
#mw_rast_crops <- terra::rast("rasters/midwest_crops_classed.tif")
crops <- terra::vect("shapefiles/midwest_crops_vec.shp")

## Running to get plot and map vectors:
ens_rast <- process_ensemble_members(dir = dir,
                                     var = var,
                                     year = 2021,
                                     crops = crops,
                                     cell_size = cell_size)

# run for towns:
towns <- carbon_uncertainty_wrapper(ens_rast,
                                    crops = crops,
                                    agg_reg = agg_twn,
                                    n_regions = n_towns)
# run for counties:
county <- carbon_uncertainty_wrapper(ens_rast,
                                     crops = crops,
                                     agg_reg = agg_counties, 
                                     n_regions = n_counties)
# run for state:
state <- carbon_uncertainty_wrapper(ens_rast,
                                    crops = crops,
                                    agg_reg = agg_states,
                                    n_regions = n_states)
# run for region:
reg <- carbon_uncertainty_wrapper(ens_rast,
                                  crops = crops,
                                  agg_reg = agg_region,
                                  n_regions = n_reg)
# clean up:
#rm(ens_rast)

# Before moving to plots/maps you should have:
# 1 - a SpatVector for each regional aggregation (town, county, state, region)
# 2 - a rast_crops 

### ARCHIVE###
# # crop the raster to study area:
# rast_crops <- terra::crop(rast_crops, cornbelt)
# # apply mask:
# rast_crops_mask <- terra::mask(rast_crops, cornbelt)
# # make a croplands area raster:
# rast_crops_mask[rast_crops_mask != 2] <- NA
# rast_crops_mask[rast_crops_mask == 2] <- 1
# # save for later:
# #writeRaster(rast_crops_mask, "rasters/midwest_crops_classed.tif")
# # make a crop layer vector:
# rast_agg <- aggregate(rast_crops_mask, fact = 5)  # gathering 5 pixels together
# rast_poly <- as.polygons(rast_agg, dissolve = TRUE)
# # save for later:
# #writeVector(rast_poly, "shapefiles/midwest_crops_vec.shp")

# test is_crop function:
#test <- get_is_crop(rast_crops_mask)
#test <- get_is_crop(mw_rast_crops)

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