### EDF SD Map Figures Script

# load libraries
library(terra)
library(sf)
library(ggplot2)
library(patchwork)

# variable names for plots:
var_names <- c("AGB", "SOC", "LAI", "SMF")
# states for additional background:
us_states <- tigris::states(cb = TRUE)

# map function:
#'@param var = numeric, number of variable name object you want - 2 for SOC
#'@param vec = regional vector from ensemble vs naive processing step - e.g. _county, town, etc
make_region_maps <- function(var, vec){
  # name plot by variable
  plot_var_name <- var_names[var]
  
  # states for additional background:
  us_states <- vect(tigris::states(cb = TRUE))
  us_states <- project(us_states, vec)
  us_sf <- st_as_sf(us_states)
  
  # when I have to make plots
  map_palette = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
  min_value = min(vec$crop_Tot_SD)
  max_value = max(vec$crop_ensVar_SD)
  breaks = c(0, 10^seq(log10(min_value), log10(max_value), length.out = length(map_palette)))
  #breaks = c(0, seq(from = min_value, to = max_value, length.out = length(map_palette)))
  #breaks = c(0,10^seq(0,log10(1000),length=9))
  
  ### Making map using terra, sf, and ggplot2:
  # convert terra SpatVector to sf object:
  sf_vec <- st_as_sf(vec)
  # add breaks:
  sf_vec$naive_bin <- cut(sf_vec$crop_Tot_SD, 
                          breaks = breaks, 
                          include.lowest = TRUE)
  sf_vec$ens_bin <- cut(sf_vec$crop_ensVar_SD, 
                        breaks = breaks, 
                        include.lowest = TRUE)
  # generate bin labels:
  labels <- levels(sf_vec$ens_bin) %>%
    gsub("\\(|\\[|\\]", "", .) %>%
    gsub(",", "-", .) %>%
    # Split each label at "-" and format each number nicely
    sapply(function(x) {
      parts <- str_split(x, "-", simplify = TRUE)
      parts_num <- format(as.numeric(parts), scientific = FALSE, trim = TRUE)
      paste(parts_num, collapse = "-")
    }) %>%
    # Convert back to a character vector
    as.character()
  
  # get proper extent:
  ext <- ext(sf_vec)
  
  # plot naive:
  map1 <- ggplot(sf_vec) +
    geom_sf(data = us_sf) +
    geom_sf(aes(fill = naive_bin)) +
    geom_sf(data = sf_vec, fill = NA, color = "black", size = 0.3) +
    labs(title = paste0("Naive vs. Ensemble SD calculations: ", plot_var_name)) +
    scale_fill_manual(
      values = map_palette, name = "Naive SD") +
    coord_sf(xlim <- c(ext[1], ext[2]), 
             ylim <- c(ext[3], ext[4])) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "lightblue", color = NA),
          panel.grid.major = element_line(color = "white", linewidth = 0.25, linetype = "dashed"),  # grid lines
          panel.grid.minor = element_line(color = "white", linewidth = 0.25, linetype = "dashed"))
  
  # plot ensemble:
  map2 <- ggplot(sf_vec) +
    geom_sf(data = us_sf) +
    geom_sf(aes(fill = ens_bin)) +
    geom_sf(data = sf_vec, fill = NA, color = "black", size = 0.4) +
    scale_fill_manual(
      values = map_palette, 
      name = "Aggregated SD",
      labels = labels) +
    coord_sf(xlim <- c(ext[1], ext[2]), 
             ylim <- c(ext[3], ext[4])) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "lightblue", color = NA),
          panel.grid.major = element_line(color = "white", linewidth = 0.25, linetype = "dashed"),  # grid lines
          panel.grid.minor = element_line(color = "white", linewidth = 0.25, linetype = "dashed"))
  
  # make together:
  compare_maps <- map1 + map2 + plot_layout(guides = "collect")
  return(compare_maps)
}

# Make the maps:
#town_maps <- make_region_maps(2, towns)
county_maps <- make_region_maps(2, county)
region_maps <- make_region_maps(2, reg)
#state_maps <- make_region_maps(2, state)


