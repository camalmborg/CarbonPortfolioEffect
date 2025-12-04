### Script for The Big Ratio Regression ###

## Load libraries
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(ggpmisc)
library(gt)

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
region_regr_plot <- ggplot(data = region_regr, mapping = aes(x = log10(n_pixels), y = model_fit, color = region)) +
  geom_point(aes(fill = region), size = 2) +
  geom_line() +
  stat_poly_eq(aes(label = paste(..eq.label.., "*\",  \"*", ..rr.label.., sep = ""),
                   group = region,
                   color = region),
    formula = y ~ x, 
    parse = TRUE, 
    size = 5) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = region),
    alpha = 0.25, color = NA) +
  labs(color = "Region",
       x = "Log Number of 1km Pixels in Portfolio",
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  guides(fill = "none") +
  theme_bw()
region_regr_plot


# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = n_pixels, y = model_fit, 
                                                               group = crop, color = crop, fill = crop, shape = region)) +
  geom_point(size = 2.75) +
  geom_line(linewidth = 0.15) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  scale_x_log10() +
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


## Region Regression Tables

make_reg_tables <- function(lm){
  # get summary:
  summ <- summary(lm)
  # get r2:
  rs <- round(summ$r.squared, digits = 3)
  # get p-vals:
  pvs <- signif(summ$coefficients[,4], digits = 3)
  # coefficients for intercept and terms:
  coeffs <- lm$coefficients
  
}


# model summary:
summ <- summary(region_lm)
# r2:
r2 <- round(summ$r.squared, digits = 3)
# p values:
pvs <- signif(summ$coefficients[,4], digits = 3)
# coefficients:
coeffs <- region_lm$coefficients

# make data frame:
table_data <- as.data.frame(t(data.frame(coeffs, pvs)))
colnames(table_data) <- c("Intercept", "Size (N pixels)", "Region", "Size x Region")
rownames(table_data) <- c("Coefficients", "P-Value")
gt(table_data)


## Tables for reporting
library(emmeans)

## For Regions
# slopes for each region:
reg_slopes <- emtrends(region_lm, ~ region, var = "log10(agg_n)")
# get p-values:
reg_slopes_summary <- as.data.frame(summary(reg_slopes, infer = TRUE))
# convert to data frame to make table:
reg_slopes <- as.data.frame(reg_slopes)
# compute intercepts:
reg_intercepts <- as.data.frame(emmeans(region_lm, ~ region))

# Combine into table
reg_regr_table <- data.frame(
  region = c("California", "Midwest"),
  slope = reg_slopes$`log10(agg_n).trend`,
  slope_se = reg_slopes$SE,
  p_value = reg_slopes_summary$p.value,
  int = reg_intercepts$emmean,
  int_se = reg_intercepts$SE
) %>%
  # round to third digit:
  mutate(across(where(is.numeric) & !matches("p_value"), ~ round(., 3))) %>%
  # add nicer p-value reporting:
  mutate(p_value = ifelse(p_value < 0.001, ">.001", round(p_value, 3)))

rrt <- reg_regr_table %>%
  select(region, slope, p_value, int) %>%
  gt() %>%
  cols_label(
    region = md("**Region**"),
    slope = md("**Slope**"),
    p_value = md("**P-value**"),
    int = md("**Intercept**")
  )
rrt

gt(reg_regr_table)

## For Crop Types
# slopes for each crop:
crop_slopes <- emtrends(crop_lm, ~ crop, var = "log10(agg_n)")
# get p-values:
crop_slopes_summary <- as.data.frame(summary(crop_slopes, infer = TRUE))
# convert to data frame to make table:
crop_slopes <- as.data.frame(crop_slopes)
# compute intercepts:
crop_intercepts <- as.data.frame(emmeans(crop_lm, ~ crop))

# Combine into table
crop_regr_table <- data.frame(
  crop = c("Corn", "Deciduous Tree Crops", "Citrus", "Grassland/Pasture (MW)", "Pasture (CA)", "Soybeans", "Field/Row Crops", "Vineyards"),
  slope = crop_slopes$`log10(agg_n).trend`,
  slope_se = crop_slopes$SE,
  p_value = crop_slopes_summary$p.value,
  int = crop_intercepts$emmean,
  int_SE = crop_intercepts$SE
  ) %>%
  # round to third digit:
  mutate(across(where(is.numeric) & !matches("p_value"), ~ round(., 3))) %>%
  # add nicer p-value reporting:
  mutate(p_value = ifelse(p_value < 0.001, ">.001", round(p_value, 3)))

crt <- crop_regr_table %>%
  select(crop, slope, p_value, int) %>%
  gt() %>%
  cols_label(
    crop = md("**Crop**"),
    slope = md("**Slope**"),
    p_value = md("**P-value**"),
    int = md("**Intercept**")
    )
crt

# # (2) California crops:
# ca_crop_regr_plot <- ggplot(data = ca_crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
#                                                                group = crop, color = crop, fill = crop)) +
#   geom_point() +
#   geom_line(size = 0.5) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
#   #scale_x_log10() +
#   labs(color = "Crop",
#        x = "Log Number of 1km Pixels in Portfolio", 
#        y = "Log(Ratio of Ensemble SD : Naive SD)") +
#   scale_color_discrete(
#     labels = c("aa_decid" = "Deciduous Tree Crops", 
#                "citrus" = "Citrus", 
#                "pasture" = "Pasture",
#                "truck_field_grain" = "Field/Row Crops",
#                "vineyd" = "Vineyards")
#   ) + 
#   guides(fill = "none") +
#   theme_bw()
# ca_crop_regr_plot

# # (3) Midwest crops:
# mw_crop_regr_plot <- ggplot(data = mw_crop_regr, mapping = aes(x = log10(n_pixels), y = model_fit, 
#                                                                group = crop, color = crop, fill = crop)) +
#   geom_point() +
#   geom_line(size = 0.5) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
#   #scale_x_log10() +
#   labs(color = "Crop",
#        x = "Log Number of 1km Pixels in Portfolio", 
#        y = "Log(Ratio of Ensemble SD : Naive SD)") +
#   scale_color_discrete(
#     labels = c("aa_corn" = "Corn", 
#                "grass_pasture" = "Grassland/Pasture", 
#                "soybeans" = "Soybeans")
#   ) + 
#   guides(fill = "none") +
#   theme_bw()
# mw_crop_regr_plot


