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


## Tables for reporting
# function for saving lm summaries:
make_reg_tables <- function(lm){
  # get summary:
  summ <- summary(lm)
  # get r2:
  r2 <- summ$r.squared
  # get p-vals:
  pvs <- summ$coefficients[,4]
  # coefficients for intercept and terms:
  coeffs <- summ$coefficients[,1]
  # standard errors:
  std_err <- summ$coefficients[,2]
  # make a list for turning into table:
  table_summary <- list(summary = summ, 
                        r2 = r2, 
                        coef = coeffs, 
                        p = pvs, 
                        se = std_err)
  # output:
  return(table_summary)
}

# For Regions:
reg_summ <- make_reg_tables(region_lm)
# Combine into table
reg_regr_table <- data.frame(
  region = c("California", "Midwest"),
  slope = c(reg_summ[["coef"]][2], (reg_summ[["coef"]][2] + reg_summ[["coef"]][4])),
  int = c(reg_summ[["coef"]][1], (reg_summ[["coef"]][1] + reg_summ[["coef"]][3])),
  p_value_slopes = c(reg_summ[["p"]][2], reg_summ[["p"]][4]),
  p_value_int = c(reg_summ[["p"]][1], reg_summ[["p"]][3]),
  se_slope = c(reg_summ[["se"]][2], reg_summ[["se"]][4]),
  se_int = c(reg_summ[["se"]][1], reg_summ[["se"]][3])
) %>%
  # round to third digit:
  mutate(across(where(is.numeric) & !matches("p_value_slopes") & !matches("p_value_int"), ~ round(., 3))) %>%
  # add nicer p-value reporting:
  mutate(p_value_slopes = ifelse(p_value_slopes < 0.001, ">.001", round(p_value_slopes, 3))) %>%
  mutate(p_value_int = ifelse(p_value_int < 0.001, ">.001", round(p_value_int, 3)))

rrt <- reg_regr_table %>%
  select(region, slope, p_value_slopes, int, p_value_int) %>%
  gt() %>%
  cols_label(
    region = md("**Region**"),
    slope = md("**Slope**"),
    p_value_slopes = md("P-value"),
    int = md("**Intercept**"),
    p_value_int = md("P-value")
  )
rrt
# save:
rrt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/region_reg_table.html")
rrt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/region_reg_table.docx")


# For Crop Types:
crop_summ <- make_reg_tables(crop_lm)
# slopes:
crop_slopes <- crop_summ[["coef"]][grep("agg_n", names(crop_summ[["coef"]]))]
crop_slopes <- c(crop_slopes[1], crop_slopes[-1] + crop_slopes[1])
# intercepts:
crop_int <- crop_summ[["coef"]][grep("agg_n", names(crop_summ[["coef"]]), invert = TRUE)]
crop_int <- c(crop_int[1], crop_int[-1] + crop_int[1])
# p-values:
slope_p <- crop_summ[["p"]][grep("agg_n", names(crop_summ[["p"]]))]
int_p <- crop_summ[["p"]][grep("agg_n", names(crop_summ[["p"]]), invert = TRUE)]
# se:
slope_se <- crop_summ[["se"]][grep("agg_n", names(crop_summ[["se"]]))]
int_se <- crop_summ[["se"]][grep("agg_n", names(crop_summ[["se"]]), invert = TRUE)]

# Combine into table
crop_regr_table <- data.frame(
  crop = c("Corn", "Deciduous Tree Crops", "Citrus", "Grassland/Pasture (MW)", "Pasture (CA)", "Soybeans", "Field/Row Crops", "Vineyards"),
  slope = crop_slopes,
  p_value_slopes = slope_p,
  slope_se = slope_se,
  int = crop_int,
  p_value_int = int_p,
  int_se = int_se
) %>%
  # round to third digit:
  mutate(across(where(is.numeric) & !matches("p_value_slopes") & !matches("p_value_int"), ~ round(., 3))) %>%
  # add nicer p-value reporting:
  mutate(p_value_slopes = ifelse(p_value_slopes < 0.001, ">.001", round(p_value_slopes, 3))) %>%
  mutate(p_value_int = ifelse(p_value_int < 0.001, ">.001", round(p_value_int, 3)))

crt <- crop_regr_table %>%
  select(crop, slope, p_value_slopes, int, p_value_int) %>%
  gt() %>%
  cols_label(
    crop = md("**Crop**"),
    slope = md("**Slope**"),
    p_value_slopes = md("P-value"),
    int = md("**Intercept**"),
    p_value_int = md("P-value")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey85")
    ),
    locations = cells_body(
      rows = 1)
  ) %>%
  tab_footnote(
    footnote = "Reference Crop Class",
    locations = cells_body(columns = crop, rows = 1),
    placement = c("right")
  )
crt
# save:
crt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/crop_reg_table.html")
crt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/CA_MW_portfolio_runs/crop_reg_table.docx")


## Making plots
# (1) regional differences:
region_regr_plot <- ggplot(data = region_regr, mapping = aes(x = n_pixels, y = model_fit, color = region)) +
  geom_point(aes(fill = region), size = 3) +
  geom_line(linewidth = 0.5) +
  stat_poly_eq(aes(label = paste(..eq.label..),#, "*\",  \"*", ..rr.label.., sep = ""),
                   group = region,
                   color = region),
    formula = y ~ x, 
    parse = TRUE, 
    size = 7) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = region),
    alpha = 0.25, color = NA) +
  scale_x_log10(breaks = c(unique(region_regr$n_pixels)), labels = scales::comma) +
  labs(color = "Region",
       x = "Number of 1km Pixels in Portfolio",
       y = "Log(Ratio of Ensemble SD : Naive SD)") +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text=element_text(size = 12))
region_regr_plot

# save the plot:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/"
# Save the plot to a PNG file:
ggsave(paste0(save_dir, "Portfolio_Plots/", Sys.Date(), "_portfolios_region_regression_plot.png"),
       plot = region_regr_plot,
       width = 10, height = 6,
       dpi = 600)

# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = n_pixels, y = model_fit, 
                                                               group = crop, color = crop, fill = crop, shape = region)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  scale_x_log10(breaks = c(unique(region_regr$n_pixels)), labels = scales::comma) +
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
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text=element_text(size = 12))
all_crop_regr_plot

# Save the plot to a PNG file:
ggsave(paste0(save_dir, "Portfolio_Plots/", Sys.Date(), "_portfolios_crops_regression_plot.png"),
       plot = region_regr_plot,
       width = 10, height = 6,
       dpi = 600)



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
