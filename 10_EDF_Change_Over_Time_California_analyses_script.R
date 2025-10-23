### Script for running change over time analyses ###

## Load libraries
library(dplyr)
library(terra)
library(sf)

## Load functions and data
# functions for portfolio processing and analysis:
source("01_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")
source("01_EDF_SDA_Portfolio_Sampling_functions_script.R")

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
# load crops:
crops <- vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")


## Process change over time product as with individual years:
# C variables:
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
dir <- cot
ens_rast <- process_ensemble_members(dir = dir,
                                     var = var,
                                     year = yr_two,
                                     crops = crops,
                                     cell_size = cell_size)

## Make polygons of particular crop classes
# convert terra vector to sf for easier separation:
crops_sf <- st_as_sf(crops) 
# separate some groups:
# citrus crops:
citrus <- crops_sf |>
  # citrus crop type:
  filter(CLASS2 == "C") |>
  # choose parcels with 100% coverage:
  filter(PCNT2 == "00")
# deciduous fruits and nuts:
decid <- crops_sf |>
  filter(CLASS2 == "D") |>
  filter(PCNT2 == "00")
# pasture crops:
pasture <- crops_sf |>
  filter(CLASS2 == "P") |>
  filter(PCNT2 == "00")
# truck crops:
truck_field_grain <- crops_sf |>
  filter(CLASS2 == c("T", "F", "G")) |>
  filter(PCNT2 == "00")
# vineyards:
vineyd <- crops_sf |>
  filter(CLASS2 == "V")|>
  filter(PCNT2 == "00")

crop_group_list <- list(citrus = citrus, 
                        decid = decid, 
                        pasture = pasture, 
                        truck_field_grain = truck_field_grain,
                        vineyd = vineyd)



