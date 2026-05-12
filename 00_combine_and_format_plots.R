# combining plots script for making nicer plots:

## Load libraries
library(ggplot2)
library(patchwork)

## Load maps and combine

# SD vs area maps:
SD_vs_area_CA <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_SD_vs_area_California.rds") +  
theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())
SD_vs_area_MW <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_SD_vs_area_Midwest.rds")

sva_combine <- (SD_vs_area_CA / SD_vs_area_MW) +
  plot_layout(guides = "collect")
sva_combine
