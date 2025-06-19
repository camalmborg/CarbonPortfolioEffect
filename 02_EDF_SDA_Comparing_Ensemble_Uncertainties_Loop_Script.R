#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN CALIFORNIA ####

## Prepare workspace ##
# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

# load libraries:
library(sf)
library(terra)

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

## Function for running analyses ##
#'@param 
compare_C_uncertainty <- function(){
  
}