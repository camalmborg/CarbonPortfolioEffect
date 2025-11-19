### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)

## Load and prep crop portfolios
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

## Regressions
# region:
region_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(region) + (log10(agg_n)*as.factor(region)), data = port_df)

# for crop types:
crop_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(crop) + (log10(agg_n)*as.factor(crop)), data = port_df)
# crop types for CA:
CA_crop_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(crop) + (log10(agg_n)*as.factor(crop)), data = ca_port_df)
# crop types for MW:
MW_crop_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(crop) + (log10(agg_n)*as.factor(crop)), data = mw_port_df)

## Add predicting values back to datasets for plots
# for region:
reg_pred <- as.data.frame(predict(region_lm, interval = "confidence"))
# for crop:
crop_pred <- as.data.frame(predict(crop_lm, interval = "confidence"))
# for CA:
ca_crop_pred <- as.data.frame(predict(CA_crop_lm, interval = "confidence"))
# for MW:
mw_crop_pred <- as.data.frame(predict(MW_crop_lm, interval = "confidence"))

## Preparing data for the plots
# (1) for uncertainty ratio vs pixels by region:
region_regr <- data.frame(n_pixels = port_df$agg_n, region = port_df$region, ratio_rev = port_df$ratio_rev, 
                          model_fit = reg_pred$fit, upper = reg_pred$upr, lower = reg_pred$lwr)
# (2) for uncertainty ratio vs pixels by crop in California:
ca_crop_regr <- data.frame(n_pixels = ca_port_df$agg_n, crop = ca_port_df$crop, ratio_rev = ca_port_df$ratio_rev,
                           model_fit = ca_crop_pred$fit, upper = ca_crop_pred$upr, lower = ca_crop_pred$lwr)
# (3) for uncertainty ratio vs pixels by crop in the Midwest:
mw_crop_regr <- data.frame(n_pixels = mw_port_df$agg_n, crop = mw_port_df$crop, ratio_rev = mw_port_df$ratio_rev,
                           model_fit = mw_crop_pred$fit, upper = mw_crop_pred$upr, lower = mw_crop_pred$lwr)
# (4) for uncertainty ratio vs pixels by all crops:
crop_regr <- data.frame(n_pixels = port_df$agg_n, region = port_df$region, crop = port_df$crop, ratio_rev = port_df$ratio_rev,
                        model_fit = crop_pred$fit, upper = crop_pred$upr, lower = crop_pred$lwr)

## Making plots
# (1) regional differences:
region_regr_plot <- ggplot(data = region_regr, mapping = aes(x = log10(n_pixels), y = model_fit)) +
  geom_point(aes(group = region, color = region, fill = region), size = 2) +
  geom_line(aes(color = region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = region), alpha = 0.25, color = NA) +
  labs(color = "Region",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  guides(fill = "none") +
  theme_bw()
region_regr_plot

# (2) California crops:
ca_crop_regr_plot <- ggplot(data = ca_crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
                                                               group = crop, color = crop, fill = crop)) +
  geom_point() +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  #scale_x_log10() +
  labs(color = "Crop",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  scale_color_discrete(
    labels = c("aa_decid" = "Deciduous Tree Crops", 
               "citrus" = "Citrus", 
               "pasture" = "Pasture",
               "truck_field_grain" = "Field/Row Crops",
               "vineyd" = "Vineyards")
  ) + 
  guides(fill = "none") +
  theme_bw()
ca_crop_regr_plot

# (3) Midwest crops:
mw_crop_regr_plot <- ggplot(data = mw_crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
                                                               group = crop, color = crop, fill = crop)) +
  geom_point() +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  #scale_x_log10() +
  labs(color = "Crop",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  scale_color_discrete(
    labels = c("aa_corn" = "Corn", 
               "grass_pasture" = "Grassland/Pasture", 
               "soybeans" = "Soybeans")
  ) + 
  guides(fill = "none") +
  theme_bw()
mw_crop_regr_plot

# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
                                                               group = crop, color = crop, fill = crop, shape = region)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  #scale_x_log10() +
  labs(color = "Crop",
       shape = "Region",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  scale_color_discrete(
    labels = c("aa_decid" = "Deciduous Tree Crops", 
               "citrus" = "Citrus", 
               "pasture" = "Pasture",
               "truck_field_grain" = "Field/Row Crops",
               "vineyd" = "Vineyards",
               "aa_corn" = "Corn", 
               "grass_pasture" = "Grassland/Pasture", 
               "soybeans" = "Soybeans")
  ) +
  guides(fill = "none") +
  theme_bw()
all_crop_regr_plot


