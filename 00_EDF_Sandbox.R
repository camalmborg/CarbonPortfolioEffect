### Sandbox ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(ggridges)

## Load Functions
# run the functions script:
#source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Portfolio_Sampling_functions_script.R")

## set working directory
wd <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(wd)

## Loading Files 
# navigate to Dongchen's North America runs:
ens <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
# choose run:
run <- "SDA_8k_site/downscale_maps_analysis_lc_ts_noGEDI_debias_rf/"
# load raster cell sizes:
cell_size <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/cell_size.tif")

## Preparing for running selected variable and year across aggregate regions:
# C variable:
soc <- "TotSoilCarb_"

# choose analysis run variables:
var <- soc
yr_one <- 2019
yr_two <- 2024
dir <- paste0(ens, run)

## California Data - testing
# load crops:
crops <- vect("/projectnb/dietzelab/dietze/CARB/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")
# california extent:
cali <- tigris::states() %>%
  filter(NAME %in% c("California"))
cali <- vect(cali)
# midwest extent:
mw <- tigris::states() %>%
  filter(NAME %in% c("Illinois", "Indiana", "Iowa"))
mw <- vect(mw)
# get total area:
cali_sf <- st_as_sf(cali)
mw_sf <- st_as_sf(mw)

# convert terra vector to sf for easier separation:
crops_sf <- st_as_sf(crops) 
# deciduous fruits and nuts:
decid <- crops_sf |>
  filter(CLASS2 == "D") |>
  filter(PCNT2 == "00")
# get a sample point for comparing ensembles:
crop_group <- decid
# convert crops back to terra polygon:
crop_type <- vect(crop_group)
# project for sampling:
crop_type <- project(crop_type, "epsg:3857")
# sample:
sample_point <- spatSample(crop_type, size = 1, method = "random")
# changing crs to what the SDA rasters are:
sample_point <- project(sample_point, "epsg: 4326")
crop_type <- project(crop_type, "epsg: 4326")


## Making Rasters 
# For loading rasters:
load_rast <- function(dir, var, yr, nfile){
  year <- paste0(dir, var, as.character(yr))
  file <- list.files(year)[grep(nfile, list.files(year))]
  tiff <- paste0(year, "/", file)
  rast <- terra::rast(tiff)
  return(rast)
}

## Testing between years:
n_ens = 100
old_samp <- rep(NA, n_ens)
new_samp <- rep(NA, n_ens)
for (i in 1:n_ens){
  print(i)
  # get ensemble number for grabbing file:
  ensem <- paste0("ensemble_", as.character(i), "_")
  # load maps:
  old <- load_rast(dir, var, yr_one, ensem)
  new <- load_rast(dir, var, yr_two, ensem)
  # sample:
  old_samp[i] <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2] 
  new_samp[i] <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2]
}
# unlist:
old_samp <- unlist(old_samp)
new_samp <- unlist(new_samp)

# # making into a csv for Mike:
# compare_2019_2024 <- data.frame("2019" = old_samp, "2024" = new_samp)
# write.csv(compare_2019_2024, file = "/projectnb/dietzelab/malmborg/EDF/compare_2019_2024.csv")

# IF NEEDED
# old mean:
old <- load_rast(dir, var, yr_one, "mean")
old_mean <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2]
new <- load_rast(dir, var, yr_two, "mean")
new_mean <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2]

# old std:
old <- load_rast(dir, var, yr_one, "std")
old_std <- terra::extract(old, sample_point, fun = sum, na.rm = TRUE)[2]
new <- load_rast(dir, var, yr_two, "std")
new_std <- terra::extract(new, sample_point, fun = sum, na.rm = TRUE)[2]

# naive sd:
naive <- sqrt(var(new_samp) + var(old_samp))
# ensemble sd:
ensemble <- sd(new_samp - old_samp)


### Looking at raw SDA product
# load:
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/sda.all.forecast.analysis.Rdata")
# get SOC
soc <- which(names(analysis.all[["2024-07-15"]]) == "TotSoilCarb")
cors <- mapply(cor, 
               as.data.frame(analysis.all[["2024-07-15"]][,soc]),
               as.data.frame(analysis.all[["2019-07-15"]][,soc]))
summary(cors)


### Working with inventory data ###
# directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/CA_MW_runs/"
setwd(dir)

# open file with inventory data:
ca_inv <- read.csv(paste0(dir, "2025-11-26_CA_inventory_plot_data.csv"))
mw_inv <- read.csv(paste0(dir, "2025-11-26_MW_inventory_plot_data.csv"))
# split into groups:
ca_naive <- ca_inv[ca_inv$variable == "crop_Tot_SD",]
ca_ens <- ca_inv[ca_inv$variable == "crop_ensVar_SD",]
mw_naive <- mw_inv[mw_inv$variable == "crop_Tot_SD",]
mw_ens <- mw_inv[mw_inv$variable == "crop_ensVar_SD",]

# learn about them:
ca_del_pc <- (ca_ens$delta/ca_naive$value)*100

# ca inventory data ranges of SDs
ca_naive_range <- ca_naive$value[which(ca_naive$type == "State(s)")]
ca_ens_range <- ca_ens$value[which(ca_naive$type == "State(s)")]
#  mw inventory data ranges of SDs
mw_naive_range <- mw_naive$value[which(mw_naive$value > 0 & mw_naive$type == "State(s)")]
mw_ens_range <- mw_ens$value[which(mw_ens$value > 0 & mw_ens$type == "State(s)")]

# factor off for largest spatial scale (state(s)):
ca_ens_range/ca_naive_range
mw_ens_range/mw_naive_range


### Working with portfolio data ###
# set working directory for static (single year) portfolios:
# dir <- "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/Portfolios/"
# setwd(dir)
# set COT portfolios working directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/Portfolios/"
setwd(dir)
# portfolio files:
port_df <- list.files(dir) %>%
  # open:
  lapply(read_csv, show_col_types = FALSE) %>%
  # row bind:
  bind_rows() %>%
  # add ratios:
  mutate(ratio = crop_Tot_SD/crop_ensVar_SD) %>%
  mutate(ratio_rev = crop_ensVar_SD/crop_Tot_SD) %>%
  # change names to make most abundant crops for each region reference classes for their regions:
  mutate(crop = case_when(crop == "decid" ~ "aa_decid",
                          crop == "corn" ~ "aa_corn",
                          TRUE ~ crop)) %>%
  filter(complete.cases(.))

# make one for California:
ca_port_df <- port_df %>%
  filter(region == "CA")
# one for the midwest:
mw_port_df <- port_df %>%
  filter(region == "MW")

# for largest spatial scales:
ca_naive_big_port <- ca_port_df[ca_port_df$agg_n == 10000,]$crop_Tot_SD
ca_ens_big_port <- ca_port_df[ca_port_df$agg_n == 10000,]$crop_ensVar_SD
mw_naive_big_port <- mw_port_df[mw_port_df$agg_n == 10000,]$crop_Tot_SD
mw_ens_big_port <- mw_port_df[mw_port_df$agg_n == 10000,]$crop_ensVar_SD
# divide:
fac_off_ca <- ca_ens_big_port/ca_naive_big_port
fac_off_mw <- mw_ens_big_port/mw_naive_big_port
# ranges:
range(fac_off_ca)
range(fac_off_mw)
