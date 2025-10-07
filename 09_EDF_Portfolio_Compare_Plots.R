### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(tidyverse)
library(sf)
library(terra)

## Load crop portfolios
# set working directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/Portfolios/"
setwd(dir)
# portfolio files:
port_files <- list.files(dir)
# open and rbind
test <- read.csv(port_files[1])
