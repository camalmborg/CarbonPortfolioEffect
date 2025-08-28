### MIDWEST RUN ###

# run the California processing script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/02_EDF_Midwest_States_Processing_script.R")

# run the plotting script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/03_EDF_SD_vs_Area_Plots_script.R")

# run the map script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/04_EDF_SD_Maps_script.R")


# save the plots and maps:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/"

# Save the plot to a PNG file:
ggsave(paste0(save_dir, "Plots/", Sys.Date(), "_MW_aggregate_regions_plot.png"),
       plot = SD_vs_area,
       dpi = 600)

# Save the maps to PNG files:
ggsave(paste0(save_dir, "Maps/", Sys.Date(), "_MW_town_maps.png"), 
       plot = town_maps,
       dpi = 600)

ggsave(paste0(save_dir, "Maps/", Sys.Date(), "_MW_county_maps.png"), 
       plot = county_maps,
       dpi = 600)

ggsave(paste0(save_dir, "Maps/", Sys.Date(), "_MW_region_maps.png"), 
       plot = region_maps,
       dpi = 600)

ggsave(paste0(save_dir, "Maps/", Sys.Date(), "_MW_state_maps.png"), 
       plot = state_maps,
       dpi = 600)
