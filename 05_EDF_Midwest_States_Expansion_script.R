# Midwest region data

# set directory
dir <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(dir)

# load libraries
library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(tigris)

# load shapefile for counties:
filename <- "shapefiles/tl_2024_us_county.shp"
# load counties vector:
us_counties <- terra::vect(filename)

# testing tigris:
cornbelt <- counties(c("Illinois", "Indiana", "Iowa"))
# Note FIPS codes for states: 17 = Illinois, 18 = Indiana, 19 = Iowa
