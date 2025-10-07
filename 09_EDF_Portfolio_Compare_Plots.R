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
