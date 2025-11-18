### SD vs Area plots for EDF project

library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)

# variable names for plots:
var_names <- c("AGB", "SOC", "LAI", "SMF")
types <- c("Town", "County", "Region", "State")

# function for preparing plot data:
#'@param agg_vector = aggregate area vector product with SD calculations
#'@param type = character indicating the spatial region, e.g. "county" - or from types vector object
prepare_plot_data <- function(agg_vector, type, crops){
  vec <- agg_vector
  # get areas of each polygon:
  vec$area_m2 <- terra::expanse(vec, unit = "m")
  # add column for type of aggregate region:
  vec$type <- type
  # reproject crops to match regional vector:
  crop_vec <- project(crops, vec)
  # calculate overlap:
  overlap <- terra::intersect(vec, crop_vec)
  # calculate expanse of overlap in each regional unit:
  vec$crops_area_m2 <- terra::expanse(overlap, unit = "m")
  # calculate the delta between naive and ensemble calculations:
  vec$delta <- vec$crop_ensVar_SD - vec$crop_Tot_SD
  # calculate ratio between naive and ensemble calculations:
  vec$ratio <- vec$crop_Tot_SD / vec$crop_ensVar_SD
  # other direction ratio:
  vec$ratio_rev <- vec$crop_ensVar_SD / vec$crop_Tot_SD
  return(vec)
}

## Get process each regional aggregate:
towns <- prepare_plot_data(agg_vector = towns,
                         type = types[1],
                         crops = crops)

county <- prepare_plot_data(agg_vector = county, 
                         type = types[2],
                         crops = crops)

reg <- prepare_plot_data(agg_vector = reg,
                         type = types[3],
                         crops = crops)

state <- prepare_plot_data(agg_vector = state,
                        type = types[4], 
                        crops = crops)

# combine data for the plots:
#same_cols <- intersect(names(county), names(towns))
same_cols <- c("mean","cropMean", "cropTot", "crop_Tot_CV", "crop_Tot_SD", "crop_ensVar_SD",
               "area_m2", "type", "crops_area_m2", "delta", "ratio", "ratio_rev")
vec <- rbind(towns[same_cols], county[same_cols], reg[same_cols], state[same_cols])

# set name for labeling plots:
plot_var_name <- var_names[2]

### Plot crop area by county vs SD:
# coerce to data.frame for plot:
plot_data <- as.data.frame(vec) %>%
  # select columns:
  select(c(area_m2, crop_Tot_SD, crop_ensVar_SD, crops_area_m2, delta, ratio, ratio_rev, type)) %>%
  # pivot longer:
  pivot_longer(
    cols = c(crop_Tot_SD, crop_ensVar_SD),
    names_to = "variable",
    values_to = "value"
  ) %>%
  arrange(factor(type, levels = c("Town", "County", "Region", "State")))

# color palette:
plot_palette <- c("orchid4", "chocolate3")

SD_vs_area <- ggplot(plot_data, aes(x = log10(area_m2), y = log10(value), color = variable, fill = variable, shape = type)) +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5, alpha = 0.15) +
  ggtitle(paste0("Naive vs. Ensemble SD calculations: ", plot_var_name)) +
  labs(x = "Area (square meters)",
       y = "SD", 
       color = "SD Calculation",
       fill = "SD Calculation") +
  scale_color_manual(values = plot_palette, 
                     labels = c("Naive", "Ensemble")) +
  scale_fill_manual(values = plot_palette, 
                    labels = c("Naive", "Ensemble")) +
  #scale_x_log10() +
  #scale_y_log10() +
  theme_bw()
# view:
#SD_vs_area

delta_vs_area <- ggplot(plot_data, aes(x = log10(area_m2), y = log10(delta), color = variable, fill = variable, shape = type)) +
  geom_point(size = 1.25, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "navy", linewidth = 0.5, alpha = 0.15) +
  ggtitle(paste0("Ensemble - Naive (Delta Plot): ", plot_var_name)) +
  labs(x = "Area (square meters)",
       y = "Ensemble SD - Naive SD") +
  #scale_x_log10() +
  #scale_y_log10() +
  theme_bw() +
  theme(legend.position = "none")

#delta_vs_area

ratio_vs_area <- ggplot(plot_data, aes(x = log10(area_m2), y = log10(ratio_rev), color = variable, fill = variable, shape = type)) +
  geom_point(size = 1.25, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "navy", linewidth = 0.5, alpha = 0.15) +
  ggtitle(paste0("Ensemble - Naive (Ratio Plot): ", plot_var_name)) +
  labs(x = "Area (square meters)",
       y = "Ratio of Total SD:Ensemble SD") +
  #scale_x_log10() +
  #scale_y_log10() +
  theme_bw() +
  theme(legend.position = "none")

#ratio_vs_area

# plot(cty$crops_area_m2, cty$crop_ensVar_SD, pch = 16, col = "blue",
#      xlab = "total crop area per county (m^2)", ylab = "SD")
# points(cty$crops_area_m2, cty$crop_Tot_SD, pch = 16, col = "red")




### ARCHIVE ###
# # choose vector object with naive and ensemble data:
# twn <- ca_towns
# cty <- ca_county
# reg <- ca_reg
# st <- ca_state

## SD vs aggregation area plots
# # find county areas in m^2:
# twn$area_m2 <- terra::expanse(twn, unit = "m")
# cty$area_m2 <- terra::expanse(cty, unit = "m")
# reg$area_m2 <- terra::expanse(reg, unit = "m")
# st$area_m2 <- terra::expanse(st, unit = "m")

# # add what each is:
# cty$type <- "county"
# twn$type <- "township"


# # function for making overlap object:
# make_crop_factor <- function(ens_rast, crops){
#   # get is_crop to make crops logical:
#   is_crop <- get_crop(ens_rast[[1]], crops)
#   # make factor:
#   reclass_iscrop <- as.factor(is_crop)
#   reclass_iscrop <- ifel(is_crop == 1, 1, NA)
#   # make polygons:
#   crop_poly <- as.polygons(reclass_iscrop)
#   # return:
#   return(crop_poly)
# }

# # find overlap between crop polygons and counties:
# #test <- get_is_crop(mw_rast_crops)
# #overlap <- terra::intersect(vec, rast_poly)
# overlap <- terra::intersect(cty, rast_poly)
# overlap <- terra::intersect(twn, rast_poly)
# # calculate overlap areas:
# cty$crops_area_m2 <- terra::expanse(overlap, unit = "m")
# #twn$crops_area_m2 <- terra::expanse(overlap, unit = "m")