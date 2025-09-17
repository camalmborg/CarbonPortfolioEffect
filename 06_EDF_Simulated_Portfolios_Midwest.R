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
# assign new crs:

# aggregate to 1km:
crop_types_agg <- aggregate(crop_type)
