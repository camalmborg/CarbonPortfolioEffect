### Sandbox ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)

## Load Functions
# run the functions script:
#source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Portfolio_Sampling_functions_script.R")

## set working directory
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files 
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_rf/"
# load raster cell sizes:
cell_size <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/cell_size.tif")

## Preparing for running selected variable and year across aggregate regions:
# C variable:
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
yr_one <- 2019
yr_two <- 2024
dir <- paste0(ens, run)

## California Data - testing
# load crops:
crops <- vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")
# california extent:
cali <- tigris::states() %>%
  filter(NAME %in% c("California"))
cali <- vect(cali)
# convert terra vector to sf for easier separation:
crops_sf <- st_as_sf(crops) 
# deciduous fruits and nuts:
decid <- crops_sf |>
  filter(CLASS2 == "D") |>
  filter(PCNT2 == "00")
# get a sample point for comparing ensembles:
crop_group <- decid
# convert crops back to terra polygon:
crop_type <- vect(crop_group)
# project for sampling:
crop_type <- project(crop_type, "epsg:3857")
# sample:
sample_point <- spatSample(crop_type, size = 1, method = "random")
# changing crs to what the SDA rasters are:
sample_point <- project(sample_point, "epsg: 4326")
crop_type <- project(crop_type, "epsg: 4326")


## Making Rasters 
# For loading rasters:
load_rast <- function(dir, var, yr, nfile){
  year <- paste0(dir, var, as.character(yr))
  file <- list.files(year)[grep(nfile, list.files(year))]
  tiff <- paste0(year, "/", file)
  rast <- terra::rast(tiff)
  return(rast)
}

## Testing between years:
n_ens = 100
old_samp <- rep(NA, n_ens)
new_samp <- rep(NA, n_ens)
for (i in 1:n_ens){
  print(i)
  # get ensemble number for grabbing file:
  ensem <- paste0("ensemble_", as.character(i), "_")
  # load maps:
  old <- load_rast(dir, var, yr_one, ensem)
  new <- load_rast(dir, var, yr_two, ensem)
  # sample:
  old_samp[i] <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2] 
  new_samp[i] <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2]
}
# unlist:
old_samp <- unlist(old_samp)
new_samp <- unlist(new_samp)

# IF NEEDED
# # old mean:
# old <- load_rast(dir, var, yr_one, "mean")
# old_mean <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2] 
# new <- load_rast(dir, var, yr_two, "mean")
# new_mean <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2] 
# 
# # old std:
# old <- load_rast(dir, var, yr_one, "std")
# old_std <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2] 
# new <- load_rast(dir, var, yr_two, "std")
# new_std <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2] 
