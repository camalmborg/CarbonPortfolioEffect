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
  # adding the line equations:
  stat_poly_eq(aes(x = n_pixels, y = model_fit, label = paste(..eq.label.., "*\",  \"*", ..rr.label.., sep = ""),
                   group = region,
                   color = region),
               formula = y ~ x, 
               parse = TRUE, 
               size = 5) +
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
       x = "Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD:Naive SD)") +
  guides(fill = "none") +
  theme_bw()
region_regr_plot

# save the plot:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/"
# Save the plot to a PNG file:
ggsave(paste0(save_dir, "Change_Over_Time_Plots/", Sys.Date(), "_COT_region_regression_plot.png"),
       plot = region_regr_plot,
       width = 10, height = 6,
       dpi = 600)


# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = n_pixels, y = model_fit, 
                                                             group = crop, color = crop, fill = crop, shape = region)) +
  geom_point() +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  scale_x_log10() +
  labs(color = "Crop",
       shape = "Region",
       x = "Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD:Naive SD") +
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


## Tables for Reporting Results
library(emmeans)
library(webshot2)

## For Regions
# slopes for each region:
reg_slopes <- emtrends(region_lm, ~ region, var = "log10(agg_n)")
stat_slopes <- emtrends(static_lm, ~region, var = "log10(agg_n)")
# get p-values:
reg_slopes_summary <- as.data.frame(summary(reg_slopes, infer = TRUE))
stat_slopes_summary <- as.data.frame(summary(stat_slopes, infer = TRUE))
# convert to data frame to make table:
reg_slopes <- as.data.frame(reg_slopes)
stat_slopes <- as.data.frame(stat_slopes)
# compute intercepts:
reg_intercepts <- as.data.frame(emmeans(region_lm, ~ region))
stat_intercepts <- as.data.frame(emmeans(static_lm, ~ region))

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

stat_regr_table <- data.frame(
  region = c("California", "Midwest"),
  slope = stat_slopes$`log10(agg_n).trend`,
  slope_se = stat_slopes$SE,
  p_value = stat_slopes_summary$p.value,
  int = stat_intercepts$emmean,
  int_se = stat_intercepts$SE
) %>%
  # round to third digit:
  mutate(across(where(is.numeric) & !matches("p_value"), ~ round(., 3))) %>%
  # add nicer p-value reporting:
  mutate(p_value = ifelse(p_value < 0.001, ">.001", round(p_value, 3)))

full_table <- rbind(reg_regr_table, stat_regr_table)

rrt <- full_table %>%
  select(region, slope, p_value, int) %>%
  gt() %>%
  cols_label(
    region = md("**Region**"),
    slope = md("**Slope**"),
    p_value = md("**P-value**"),
    int = md("**Intercept**")
  ) %>%
  tab_row_group(
    group = "2019-2024 Change Over Time Portfolios",
    rows = 1:2
  ) %>%
  tab_row_group(
    group = "2021 Static Portfolios",
    rows = 3:4
  ) %>%
  tab_style(
    style = cell_fill(color = "grey85"),
    locations = cells_row_groups(groups = c("2019-2024 Change Over Time Portfolios", "2021 Static Portfolios"))
  )
rrt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_and_stat_region_reg_table.docx")


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

crt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_crop_reg_table.html")
