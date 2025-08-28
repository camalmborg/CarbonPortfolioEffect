#### EDF PROJECT SCRIPT FOR SDA PRODUCT ANALYSES IN CALIFORNIA ####

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
#terraOptions(thread = 16)
library(dplyr)

# source functions:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/02_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")

# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files ##
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"
#run <- "SDA_25ens_GEDI_2025_5_23/downscale_maps_analysis_lc_ts/" # deprec.
# # get directories that include tiff files:
# dirs <- list.dirs(paste0(ens, run))[-grep("07-15", list.dirs(paste0(ens, run)))]

# load information for region and aggregation scale:
counties <- "/projectnb/dietzelab/dietze/CARB/CA_Counties.shp"
twnshps <- "/projectnb/dietzelab/malmborg/CARB/ca_towns/California_City_Boundaries_and_Identifiers.shp"
#hydreg <- "/projectnb/dietzelab/malmborg/CARB/LandIQ_shps/i15_Crop_Mapping_2021_SHP/LandIQ_2021_hydro_reg_aggregated.shp"
crops <- vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")

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
# county group:
agg_counties <- vect(counties)
n_counties <- length(unique(agg_counties$NAME))

# township group:
agg_towns <- vect(twnshps)
agg_towns <- agg_towns[is.na(agg_towns$OFFSHORE),]
agg_towns <- terra::project(agg_towns, agg_counties)
n_towns <- length(agg_towns$CDTFA_CITY)

# region group:
# select counties for each region:
Sac_Vall_poly <- agg_counties[agg_counties$NAME %in% c("Modoc", "Lassen", "Siskiyou", "Shasta", 
                                                       "Tehama","Glenn", "Butte", "Colusa", "Sutter", 
                                                       "Yuba", "Yolo","Sacramento", "Solano")]
San_Joaq_poly <- agg_counties[agg_counties$NAME %in% c("Plumas", "Sierra", "Nevada", "Placer", 
                                                       "El Dorado", "Alpine", "Amador", "Calaveras", 
                                                       "Mono", "Inyo", "Tulare", "Kern", "Kings", 
                                                       "Fresno", "Madera", "Merced", "Stanislaus", 
                                                       "Mariposa", "Tuolumne", "San Joaquin")]
South_CA_poly <- agg_counties[agg_counties$NAME %in% c("San Bernardino", "Riverside", "Imperial", 
                                                       "San Diego", "Orange", "Los Angeles")]
Bay_Coast_poly <- agg_counties[agg_counties$NAME %in% c("Contra Costa", "Alameda", "San Mateo", 
                                                        "Santa Clara", "Santa Cruz", "San Benito", 
                                                        "Monterey", "San Luis Obispo", "Santa Barbara", 
                                                        "Ventura")]
Nor_Coast_poly <- agg_counties[agg_counties$NAME %in% c("Del Norte", "Humboldt", "Trinity", 
                                                        "Mendocino", "Lake", "Napa", "Sonoma", 
                                                        "Marin")]

# aggregate them by dissolving:
Sac_Vall <- aggregate(Sac_Vall_poly, dissolve = TRUE)
San_Joaq <- aggregate(San_Joaq_poly, dissolve = TRUE)
South_CA <- aggregate(South_CA_poly, dissolve = TRUE)
Bay_Coast <- aggregate(Bay_Coast_poly, dissolve = TRUE)
Nor_Coast <- aggregate(Nor_Coast_poly, dissolve = TRUE)

# join them to be one polygon vector object:
agg_regions <- rbind(Sac_Vall, San_Joaq, South_CA, Bay_Coast, Nor_Coast)
rm(Sac_Vall, Sac_Vall_poly, San_Joaq, San_Joaq_poly, South_CA, South_CA_poly, 
   Bay_Coast, Bay_Coast_poly, Nor_Coast, Nor_Coast_poly)
n_reg <- nrow(agg_regions)


# tigris states:
ca_sf <- tigris::states() %>%
  filter(NAME == "California")
agg_state <- vect(ca_sf)
rm(ca_sf)
agg_state <- terra::project(agg_state, agg_counties)



## Running to get plot and map vectors:

ens_rast <- process_ensemble_members(dir = dir,
                                     var = var,
                                     year = 2021,
                                     crops = crops)

# run for counties:
ca_county <- carbon_uncertainty_wrapper(ens_rast,
                                        crops = crops,
                                        agg_reg = agg_counties, 
                                        n_regions = n_counties)
# run for towns:
ca_towns <- carbon_uncertainty_wrapper(ens_rast,
                                       crops = crops,
                                       agg_reg = agg_towns,
                                       n_regions = n_towns)

# run for regions:
ca_reg <- carbon_uncertainty_wrapper(ens_rast,
                                     crops = crops,
                                     agg_reg = agg_regions,
                                     n_regions = n_reg)

# run for state:
ca_state <- carbon_uncertainty_wrapper(ens_rast, 
                                       crops = crops,
                                       agg_reg = agg_state,
                                       n_regions = 1)





## Making larger regions from CA counties:
# county_sort <- function(counties, names) {
#   out <- counties[counties$NAME %in% names]
#   return(out)
# } didn't work but it was worth a try


### ARCHIVE ###
# # test functions:
# ens_rast <- process_ensemble_members(dir, agb, 2021, crops)
# is_crop <- get_crop(ens_rast[[1]], crops)
# Reg <- naive_C_uncertainty(ens_rast, is_crop, region)
# Reg <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)  # still need to find a good way to deal with number of counties/aggregate regions

# ### run a loop to save the outputs:
# # object of variables:
# c_vars <- c(soc)
# # empty list to fill:
# vec_list <- list()
# # run for 2021:
# for (i in c_vars){
#   print(i)
#   ens_rast <- process_ensemble_members(dir, i, 2021, crops)
#   is_crop <- get_crop(ens_rast[[1]], crops)
#   Reg <- naive_C_uncertainty(ens_rast, is_crop, agg_reg)
#   Reg <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)
#   name <- paste0("Reg_", i)
#   vec_list[name] <- Reg
#   # filename <- paste0("SDA_Uncert_Outputs/",Sys.Date(), "_CA_crops_county_SDA_uncert_",
#   #                    i, year, ".shp")
#   # writeVector(Reg, filename)
#   # write.csv(Reg, filename)
# }

