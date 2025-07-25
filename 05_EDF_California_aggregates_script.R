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
counties <- "/projectnb/dietzelab/dietze/CARB/CA_Counties.shp"
twnshps <- "/projectnb/dietzelab/malmborg/CARB/ca_towns/California_City_Boundaries_and_Identifiers.shp"
#hydreg <- "/projectnb/dietzelab/malmborg/CARB/LandIQ_shps/i15_Crop_Mapping_2021_SHP/LandIQ_2021_hydro_reg_aggregated.shp"
crops <- "/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp"
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")

# number of aggregate regions (e.g. counties): this runs for California, would need to be fixed for each place
agg_reg <- vect(region)
agg_reg <- agg_reg[is.na(agg_reg$OFFSHORE),] # only need townships and not marine buffer regions
#n_regions <- length(unique(agg_reg$NAME))  # for counties
n_regions <- length(unique(agg_reg$CDTFA_CITY))
#n_regions <- length(unique(agg_reg$HYDRO_RGN))

# # test functions:
# ens_rast <- process_ensemble_members(dir, agb, 2021, crops)
# is_crop <- get_crop(ens_rast[[1]], crops)
# Reg <- naive_C_uncertainty(ens_rast, is_crop, region)
# Reg <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)  # still need to find a good way to deal with number of counties/aggregate regions

### run a loop to save the outputs:
# object of variables:
c_vars <- c(soc)
# empty list to fill:
vec_list <- list()
# run for 2021:
for (i in c_vars){
  print(i)
  ens_rast <- process_ensemble_members(dir, i, 2021, crops)
  is_crop <- get_crop(ens_rast[[1]], crops)
  Reg <- naive_C_uncertainty(ens_rast, is_crop, agg_reg)
  Reg <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)
  name <- paste0("Reg_", i)
  vec_list[name] <- Reg
  # filename <- paste0("SDA_Uncert_Outputs/",Sys.Date(), "_CA_crops_county_SDA_uncert_",
  #                    i, year, ".shp")
  # writeVector(Reg, filename)
  # write.csv(Reg, filename)
}

#county <- vec_list
twnshp <- vec_list
#hydr_reg <- vec_list




# save as shapefiles for easy loading:
# cty <- county[[1]]
# file_name <- "shapefiles/ca_county_shp.shp"
# writeVector(cty, filename = file_name, overwrite = TRUE)

# twn <- twnshp[[1]]
# file_name <- "shapefiles/ca_twnshp.shp"
# writeVector(twn, filename = file_name, overwrite = TRUE)

# # test with different variable and year:
# ens_rast <- process_ensemble_members(dir, agb, 2022, crops)
# is_crop <- get_crop(ens_rast[[1]], crops)
# Reg_agb <- naive_C_uncertainty(ens_rast, is_crop, region)
# Reg_agb <- ensemble_C_uncertainty(ens_rast, n_regions, Reg)
