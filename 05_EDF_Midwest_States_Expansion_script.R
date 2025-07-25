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

# load raster for crops:
crop_rast <- ""

# load shapefile for counties:
filename <- "shapefiles/tl_2024_us_county.shp"
# load counties vector:
us_counties <- terra::vect(filename)

# testing tigris:
cornbelt <- counties(c("Illinois", "Indiana", "Iowa"))
# Note FIPS codes for states: 17 = Illinois, 18 = Indiana, 19 = Iowa
