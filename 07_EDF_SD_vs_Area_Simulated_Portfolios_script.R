### SD vs Area plots for EDF project - Simulated Portfolio Runs

library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)

# variable names for plots:
var_names <- c("AGB", "SOC", "LAI", "SMF")
number_of_pixels <- c("1", "10", "1000", "10,000")

# which crop group (change based on crops):
crop_portfolio = citrus_portfolios
# portfolio names for list processing:
pixel_groups <- names(crop_portfolio)

# for testing:
portfolio_list <- crop_portfolio
group = pixel_groups[3]

# function for preparing plot data:
#'@param portfolio_list = list for pixel group portfolio with SD calculations
prepare_portfolio_data <- function(portfolio_list, group){
  # select portfolio list group by number of pixels:
  port <- portfolio_list[[group]]
  # add a rep number:
  for (i in 1:length(port)){
    port[[i]]$member <- paste0("Rep_", i)
    port[[i]]$ID <- paste0("Rep", i, "_", seq_len(nrow(port[[i]])))
  }
  # make into one vector:
  rep_vec <- do.call(rbind, port)

  # calculate the delta between naive and ensemble calculations:
  rep_vec$delta <- rep_vec$crop_ensVar_SD - rep_vec$crop_Tot_SD
  # calculate ratio between naive and ensemble calculations:
  rep_vec$ratio <- rep_vec$crop_Tot_SD / rep_vec$crop_ensVar_SD
  # other direction ratio:
  rep_vec$ratio_rev <- rep_vec$crop_ensVar_SD / rep_vec$crop_Tot_SD
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

SD_vs_area <- ggplot(plot_data, aes(x = area_m2, y = value, color = variable, fill = variable, shape = type)) +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5, alpha = 0.15) +
  ggtitle(paste0("Naive vs. Ensemble SD calculations: ", plot_var_name)) +
  labs(x = "Area (square meters)",
       y = "SD", 
       color = "SD Calculation",
       fill = "SD Calculation") +
  scale_color_manual(values = plot_palette, 
                     labels = c("Ensemble", "Naive")) +
  scale_fill_manual(values = plot_palette, 
                    labels = c("Ensemble", "Naive")) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()
# view:
#SD_vs_area
