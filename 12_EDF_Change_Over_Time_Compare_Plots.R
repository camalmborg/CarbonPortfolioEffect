### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)

## Load and prep crop portfolios
# set static portfolios working directory:
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
  mutate(analysis = "static") %>%
  filter(complete.cases(.))

# set COT working directory:
dir <- "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/Portfolios/"
setwd(dir)
# portfolio files:
cot_port_df <- list.files(dir) %>%
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
  mutate(analysis = "cot") %>%
  filter(complete.cases(.))

# combining these for region regression plot:
stat_cot_comb <- rbind(port_df, cot_port_df)


## Regressions
# region:
region_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(region) + (log10(agg_n)*as.factor(region)), data = cot_port_df)
static_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(region) + (log10(agg_n)*as.factor(region)), data = port_df)

# for crop types:
crop_lm <- lm(log10(ratio_rev) ~ log10(agg_n) + as.factor(crop) + (log10(agg_n)*as.factor(crop)), data = cot_port_df)


## Add predicting values back to datasets for plots
# for region:
reg_pred <- as.data.frame(predict(region_lm, interval = "confidence"))
# for static:
stat_pred <- as.data.frame(predict(static_lm, interval = "confidence"))
# for crop:
crop_pred <- as.data.frame(predict(crop_lm, interval = "confidence"))


## Preparing data for the plots
# (1) for uncertainty ratio vs pixels by region:
region_regr <- data.frame(n_pixels = cot_port_df$agg_n, region = cot_port_df$region, ratio_rev = cot_port_df$ratio_rev, analysis = cot_port_df$analysis,
                          model_fit = reg_pred$fit, upper = reg_pred$upr, lower = reg_pred$lwr)
stat_regr <- data.frame(n_pixels = port_df$agg_n, region = port_df$region, ratio_rev = port_df$ratio_rev, analysis = port_df$analysis,
                          model_fit = stat_pred$fit, upper = stat_pred$upr, lower = stat_pred$lwr)
# (4) for uncertainty ratio vs pixels by all crops:
crop_regr <- data.frame(n_pixels = cot_port_df$agg_n, region = cot_port_df$region, crop = cot_port_df$crop, ratio_rev = cot_port_df$ratio_rev,
                        model_fit = crop_pred$fit, upper = crop_pred$upr, lower = crop_pred$lwr)

## Making plots
# (1) regional differences:
region_regr_plot <- ggplot(data = region_regr) +
  # plotting the sections with the cot portfolios:
  geom_point(mapping = aes(x = n_pixels, y = model_fit, 
                           group = region, 
                           color = region, 
                           fill = region)) +
  geom_line(mapping = aes(x = n_pixels, y = model_fit, 
                          group = region, 
                          color = region, 
                          linetype = "Change Over Time Portfolio"), 
            size = 0.5) +
  geom_ribbon(aes(x = n_pixels, ymin = lower, ymax = upper, 
                  fill = region, group = region), 
              alpha = 0.25, 
              color = NA) +
  # adding the plot for the static portfolios:
  geom_line(data = stat_regr, mapping = aes(x = n_pixels, y = model_fit,
                                            group = region, 
                                            color = region,
                                            linetype = "Static Portfolio")) + #,
            #linetype = "dashed") +
  scale_x_log10() +
  scale_linetype_manual(values = c("Change Over Time Portfolio" = "solid",
                                   "Static Portfolio" = "dashed")) +
  labs(color = "Region",
       linetype = "Portfolio Type",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Model Fit") +
  guides(fill = "none") +
  theme_bw()
region_regr_plot


# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
                                                             group = crop, color = crop, fill = crop, shape = region)) +
  geom_point() +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  #scale_x_log10() +
  labs(color = "Crop",
       shape = "Region",
       x = "Log Number of 1km Pixels in Portfolio", 
       y = "Model Fit") +
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


