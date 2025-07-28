### SD vs Area plots for EDF project

library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read in a vector:
#outputs_folder <- '/projectnb/dietzelab/malmborg/EDF/SDA_Uncert_Outputs/'
#files <- list.files(outputs_folder)[grep("shp",list.files(outputs_folder))]
#load("/projectnb/dietzelab/malmborg/EDF/R_Environments/2025_07_21_environment.RData")
# vector:
#vec <- vect(paste0(outputs_folder, "/", files[1]))

# variable names for plots:
var_names <- c("AGB", "SOC", "LAI", "SMF")

# choose vector object with naive and ensemble data:
var <- 1
#vec <- vec_list[[var]]
cty <- county[[var]]
twn <- twnshp[[var]]
#hyd <- hydr_reg[[var]]

plot_var_name <- var_names[2]

## SD vs aggregation area plots
# find county areas in m^2:
#vec$county_area_m2 <- terra::expanse(vec, unit = "m")
cty$area_m2 <- terra::expanse(cty, unit = "m")
twn$area_m2 <- terra::expanse(twn, unit = "m")

# add what each is:
cty$type <- "county"
twn$type <- "township"

# reclass is_crop raster object to prepare for convert to polygon:
reclass_iscrop <- as.factor(is_crop)
reclass_iscrop <- ifel(is_crop == 1, 1, NA)
# convert to polygons
rast_poly <- as.polygons(reclass_iscrop)

# find overlap between crop polygons and counties:
#overlap <- terra::intersect(vec, rast_poly)
#overlap <- terra::intersect(cty, rast_poly)
overlap <- terra::intersect(twn, rast_poly)
# calculate overlap areas:
#cty$crops_area_m2 <- terra::expanse(overlap, unit = "m")
twn$crops_area_m2 <- terra::expanse(overlap, unit = "m")

same_cols <- intersect(names(cty), names(twn))
vec <- rbind(cty[same_cols], twn[same_cols])

### Plot crop area by county vs SD:
# coerce to data.frame for plot:
plot_data <- as.data.frame(vec) %>%
  # select columns:
  select(c(area_m2, crop_Tot_SD, crop_ensVar_SD, crops_area_m2, type)) %>%
  # pivot longer:
  pivot_longer(
    cols = c(crop_Tot_SD, crop_ensVar_SD),
    names_to = "variable",
    values_to = "value"
  )

# color palette:
plot_palette <- c("orchid4", "chocolate3")

SD_vs_area <- ggplot(plot_data, aes(area_m2, value, color = variable, fill = variable, shape = type)) +
  geom_point(size = 2) +
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
SD_vs_area

# plot(vec$crops_area_m2, vec$crop_ensVar_SD, pch = 16, col = "blue",
#      xlab = "total crop area per county (m^2)", ylab = "SD")
# points(vec$crops_area_m2, vec$crop_Tot_SD, pch = 16, col = "red")

