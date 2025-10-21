### Script for making change over time maps ###

## Prepare workspace
# load libraries:
library(dplyr)
library(terra)
library(sf)
# directory:


# open each file for year 1 and year 2
# subtract year 2 from year 1 to get the change over time
# save as a new geotiff 
