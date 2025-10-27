### Script for making change over time maps ###

## Prepare workspace ##
# load libraries:
library(dplyr)
library(terra)
library(sf)
# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"

## Preparing for running selected variable and year across aggregate regions:
# C variable:
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
yr_one <- 2019
yr_two <- 2024
dir <- paste0(ens, run)
# for saving: (to be compatible with original functions and Dongchen's NA product directory naming conventions)
save_dir <- paste0(getwd(), "/Change_Over_Time/", var, as.character(yr_two), "/")

## Functions
# (1) for loading rasters:
load_rast <- function(dir, var, yr, nfile){
  year <- paste0(dir, var, as.character(yr))
  file <- list.files(year)[grep(nfile, list.files(year))]
  tiff <- paste0(year, "/", file)
  rast <- terra::rast(tiff)
  return(rast)
}

## Make new GeoTIFF files with Y2-Y1 maps
# MEAN:
# load maps:
old <- load_rast(dir, var, yr_one, "mean")
new <- load_rast(dir, var, yr_two, "mean")
# cellwise subtraction:
delta <- new - old
# save:
writeRaster(delta, file = paste0(save_dir, var, "mean_diff_", yr_one, "_", yr_two, ".tiff"), overwrite = TRUE)

# SD:
# load maps:
old <- load_rast(dir, var, yr_one, "std")
new <- load_rast(dir, var, yr_two, "std")
# cellwise difference between sd's:
diff_sd <- sqrt(old^2 + new^2)
# save:
writeRaster(diff_sd, file = paste0(save_dir, var, "std_diff_", yr_one, "_", yr_two, ".tiff"), overwrite = TRUE)

#ENSEMBLES:
n_ens = 100
for (i in 1:n_ens){
  print(i)
  # get ensemble number for grabbing file:
  ens <- paste0("ensemble_", as.character(i), "_")
  # load maps:
  old <- load_rast(dir, var, yr_one, ens)
  new <- load_rast(dir, var, yr_two, ens)
  # cellwise subtraction:
  delta <- new - old
  # save:
  writeRaster(delta, file = paste0(save_dir, var, ens,"diff_", yr_one, "_", yr_two, ".tiff"), overwrite = TRUE)
}


