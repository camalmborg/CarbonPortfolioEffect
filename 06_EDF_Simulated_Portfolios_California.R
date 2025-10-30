### Simulated portfolios ###
# Testing in CA to start

## Load Libraries
library(sf)
library(terra)
library(dplyr)

# set working directory:
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

# source processing functions:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Comparing_Ensemble_Uncertainties_functions_script.R")
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Portfolio_Sampling_functions_script.R")

## Load Data:
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"
# load raster cell sizes:
cell_size <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/cell_size.tif")
# load crops:
crops <- vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")
classes <- read.table("https://raw.githubusercontent.com/ccmmf/rs-sandbox/refs/heads/main/code_snippets/landiq_crop_mapping_codes.tsv",
                      header=TRUE, sep="\t")

## Process NA SDA product for California:
# C variables:
agb <- "AbvGrndWood_"
lai <- "LAI_"
smf <- "SoilMoistFrac_"
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
year <- 2021
dir <- paste0(ens, run)
ens_rast <- process_ensemble_members(dir = dir,
                                     var = var,
                                     year = year,
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


# ## Get all the California portfolios:
# n_pixels_citrus = c(1, 10, 100, 1000)
# n_pixels = c(1, 10, 100, 1000, 10000)

# # run the portfolios:
# citrus_portfolios <- all_portfolios_runs(crop_group = citrus,
#                                          ens_rast = ens_rast,
#                                          n_pixels_vec = n_pixels_citrus,
#                                          n_reps = 100)
# 
# decid_portfolios <- all_portfolios_runs(crop_group = decid,
#                                         ens_rast = ens_rast,
#                                         n_pixels_vec = n_pixels,
#                                         n_reps = 100)
# 
# pasture_portfolios <- all_portfolios_runs(crop_group = pasture,
#                                           ens_rast = ens_rast,
#                                           n_pixels_vec = n_pixels,
#                                           n_reps = 100)

### ARCHIVE ###
# # convert back to polygons:
# crop_type <- vect(citrus)
# # project to raster crs:
# crop_type <- project(crop_type, ens_rast[[1]])
# # crop raster to crop area:
# crop_rast <- crop(ens_rast[[1]], crop_type)
# crop_mask <- mask(crop_rast, crop_type)
# # put it in a meters-based projection:
# crop_mask <- project(crop_mask, "EPSG:3857")
# # sample pixels in crop area:
# test <- spatSample(crop_mask,
#                    size = 1000,
#                    method = "random",
#                    as.points = TRUE,
#                    #values = TRUE,
#                    na.rm = TRUE)
# 
# #test_aggr <- aggregate(test)
# r_crop <- crop(crop_mask, test)
# r_mask <- mask(r_crop, test)


# test <- bind_rows(citrus_portfolio_1px, .id = "portfolio_num")
# test <- do.call(rbind, citrus_portfolio_1px)

# citrus_portfolio_1px <- portfolio_run(crop_group = citrus,
#                                       ens_rast = ens_rast,
#                                       n_pixels = 1,
#                                       n_reps = 100)

# citrus_full_portfolio <- list()
# for(i in 1:length(n_pixels)){
#   p_name <- paste0(as.character(n_pixels[i]), "_pixel_portfolio")
#   citrus_full_portfolio[[p_name]] <- portfolio_run(crop_group = citrus,
#                                                    ens_rast = ens_rast,
#                                                    n_pixels = n_pixels[i],
#                                                    n_reps = 100)
# }

# 
# decid_full_portfolio <- all_portfolio_runs(crop_group = decid,
#                                            ens_rast = ens_rast,
#                                            n_pixels_vec = n_pixels,
#                                            n_reps = 100)

