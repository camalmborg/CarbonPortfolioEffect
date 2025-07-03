### SD vs Area plots for EDF project

library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read in a vector:
#outputs_folder <- '/projectnb/dietzelab/malmborg/EDF/SDA_Uncert_Outputs/'
#files <- list.files(outputs_folder)[grep("shp",list.files(outputs_folder))]
# vector:
#vec <- vect(paste0(outputs_folder, "/", files[1]))
#vec <- read.csv(paste0(outputs_folder,"/", files[1]))

vec <- vec_list[[1]]

## SD vs aggregation area plots
# find county areas in m^2:
vec$county_area_m2 <- terra::expanse(vec, unit = "m")

# reclass is_crop raster object to prepare for convert to polygon:
reclass_iscrop <- as.factor(is_crop)
reclass_iscrop <- ifel(is_crop == 1, 1, NA)
# convert to polygons
rast_poly <- as.polygons(reclass_iscrop)

# find overlap between crop polygons and counties:
overlap <- terra::intersect(vec, rast_poly)
# calculate overlap areas:
vec$crops_area_m2 <- terra::expanse(overlap, unit = "m")

### Plot crop area by county vs SD:
# coerce to data.frame for plot:
plot_data <- as.data.frame(vec) %>%
  # select columns:
  select(c(county_area_m2, crop_Tot_SD, crop_ensVar_SD)) %>%
  # pivot longer:
  pivot_longer(
    cols = c(crop_Tot_SD, crop_ensVar_SD),
    names_to = "variable",
    values_to = "value"
  )

# color palette:
plot_palette <- c("orchid4", "chocolate3")

SD_vs_area <- ggplot(plot_data, aes(county_area_m2, value, color = variable)) +
  geom_point() +
  ggtitle("Naive vs. Ensemble SD calculations: AGB") +
  labs(x = "Area (square meters)",
       y = "SD", 
       color = "SD Calculation") +
  scale_color_manual(values = plot_palette, 
                     labels = c("Ensemble", "Naive")) +
  theme_bw()
# view:
SD_vs_area

plot(vec$crops_area_m2, vec$crop_ensVar_SD, pch = 16, col = "blue",
     xlab = "total crop area per county (m^2)", ylab = "SD")
points(vec$crops_area_m2, vec$crop_Tot_SD, pch = 16, col = "red")
