### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(tidyverse)
library(sf)
library(terra)

## Load crop portfolios
# set working directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/"
setwd(dir)
# portfolio files:
port_files <- list.files(paste0(dir, "CA_MW_portfolio_runs/Portfolios/"))

# make a big data frame of all portfolios of all crops:
## load crop portfolio
load(paste0(dir, "CA_MW_portfolio_runs/Portfolios/", port_files[1]))
## make it a data frame
cp_list <- lapply(crop_portfolios, as.data.frame)
cp <- do.call(rbind, cp_list)
## bind all portfolios
