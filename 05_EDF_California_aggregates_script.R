#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN CALIFORNIA ####

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
#terraOptions(thread = 16)
library(dplyr)

# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/"
# # get directories that include tiff files:
# dirs <- list.dirs(paste0(ens, run))[-grep("07-15", list.dirs(paste0(ens, run)))]

# C variables:
agb <- "AbvGrndWood_"
lai <- "LAI_"
smf <- "SoilMoistFrac_"
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- 2021
dir <- paste0(ens, run)

# load information for region and aggregation scale:
#region <- "/projectnb/dietzelab/dietze/CARB/CA_Counties.shp"
region <- "/projectnb/dietzelab/malmborg/CARB/ca_towns/California_City_Boundaries_and_Identifiers.shp"
#region <- "/projectnb/dietzelab/malmborg/CARB/LandIQ_shps/i15_Crop_Mapping_2021_SHP/LandIQ_2021_hydro_reg_aggregated.shp"
crops <- "/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp"
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")