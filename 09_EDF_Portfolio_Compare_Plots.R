### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)

## Load crop portfolios
# set working directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/Portfolios/"
setwd(dir)
# portfolio files:
port_df <- list.files(dir) %>%
  # open:
  lapply(read_csv, show_col_types = FALSE) %>%
  # row bind:
  bind_rows() %>%
  # add ratios:
  mutate(ratio = crop_Tot_SD/crop_ensVar_SD) %>%
  mutate(ratio_rev = crop_ensVar_SD/crop_Tot_SD)

## Regression
# run the model:
crop_lm <- lm(ratio ~ log10(agg_n) + as.factor(crop) + as.factor(region), data = port_df)

