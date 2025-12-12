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


## Tables for Reporting Results
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
  mutate(p_value_slopes = ifelse(p_value_slopes < 0.001, "< 0.001", round(p_value_slopes, 3))) %>%
  mutate(p_value_int = ifelse(p_value_int < 0.001, "< 0.001", round(p_value_int, 3)))

reg_summ <- make_reg_tables(static_lm)
# Combine into table
stat_regr_table <- data.frame(
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
  mutate(p_value_slopes = ifelse(p_value_slopes < 0.001, "< 0.001", round(p_value_slopes, 3))) %>%
  mutate(p_value_int = ifelse(p_value_int < 0.001, "< 0.001", round(p_value_int, 3)))

full_table <- rbind(reg_regr_table, stat_regr_table)

rrt <- full_table %>%
  select(region, slope, p_value_slopes, int, p_value_int) %>%
  gt() %>%
  cols_label(
    region = md("**Region**"),
    slope = md("**Slope**"),
    p_value_slopes = md("P-value"),
    int = md("**Intercept**"),
    p_value_int = md("P-value")
  ) %>%
  tab_row_group(
    label = "2019-2024 Change Over Time Portfolios",
    rows = 1:2
  ) %>%
  tab_row_group(
    label = "2021 Static Portfolios",
    rows = 3:4
  ) %>%
  tab_style(
    style = cell_fill(color = "grey85"),
    locations = cells_row_groups(groups = c("2019-2024 Change Over Time Portfolios", "2021 Static Portfolios"))
  )
rrt
# save:
rrt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_and_stat_region_reg_table.html")
rrt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_and_stat_region_reg_table.docx")


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
  mutate(p_value_slopes = ifelse(p_value_slopes < 0.001, "< 0.001", round(p_value_slopes, 3))) %>%
  mutate(p_value_int = ifelse(p_value_int < 0.001, "< 0.001", round(p_value_int, 3)))

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
crt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_crop_reg_table.html")
crt %>% gtsave(filename = "/projectnb/dietzelab/malmborg/EDF/Change_Over_Time/cot_crop_reg_table.docx")


## Making plots
# (1) regional differences:
region_regr_plot <- ggplot(data = region_regr) +
  # plotting the sections with the cot portfolios:
  geom_point(mapping = aes(x = n_pixels, y = model_fit, 
                           group = region, 
                           color = region, 
                           fill = region),
             size = 3) +
  geom_line(mapping = aes(x = n_pixels, y = model_fit, 
                          group = region, 
                          color = region, 
                          linetype = "Change Over Time Portfolio"),
            linewidth = 0.5) +
  geom_ribbon(aes(x = n_pixels, ymin = lower, ymax = upper, 
                  fill = region, group = region), 
              alpha = 0.25, 
              color = NA) +
  # adding the line equations:
  stat_poly_eq(aes(x = n_pixels, y = model_fit, label = paste(..eq.label..),#, "*\",  \"*", ..rr.label.., sep = ""),
                   group = region,
                   color = region),
               formula = y ~ x, 
               parse = TRUE, 
               size = 7) +
  # adding the plot for the static portfolios:
  geom_line(data = stat_regr, mapping = aes(x = n_pixels, y = model_fit,
                                            group = region, 
                                            color = region,
                                            linetype = "Static Portfolio"),
            linewidth = 0.75, alpha = 0.5) + 
  scale_x_log10(breaks = c(unique(region_regr$n_pixels)), labels = scales::comma) +
  scale_y_continuous(
    breaks = log10(c(1, 10, 100, 1000)),  
    labels = c(1, 10, 100, 1000)          
  ) +
  scale_linetype_manual(values = c("Change Over Time Portfolio" = "solid",
                                   "Static Portfolio" = "dashed")) +
  labs(color = "Region",
       linetype = "Portfolio Type",
       x = "Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD:Naive SD)") +
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
ggsave(paste0(save_dir, "Change_Over_Time_Plots/", Sys.Date(), "_COT_region_regression_plot.png"),
       plot = region_regr_plot,
       width = 12, height = 6,
       dpi = 600)


# (4) all crops:
all_crop_regr_plot <- ggplot(data = crop_regr, mapping = aes(x = n_pixels, y = model_fit, 
                                                             group = crop, color = crop, fill = crop, shape = region)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = crop), alpha = 0.25, color = NA) +
  scale_x_log10(breaks = c(unique(region_regr$n_pixels)), labels = scales::comma) +
  scale_y_continuous(
    breaks = log10(c(1, 10, 100, 1000)),  
    labels = c(1, 10, 100, 1000)          
  ) +
  labs(color = "Crop",
       shape = "Region",
       x = "Number of 1km Pixels in Portfolio", 
       y = "Log(Ratio of Ensemble SD:Naive SD)") +
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

# save the plot:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/"
# Save the plot to a PNG file:
ggsave(paste0(save_dir, "Change_Over_Time_Plots/", Sys.Date(), "_COT_crop_regression_plot.png"),
       plot = all_crop_regr_plot,
       width = 11, height = 6,
       dpi = 600)
