# combining plots script for making nicer plots:

## Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

## Load maps and combine

# SD vs area maps:
SD_vs_area_CA <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_SD_vs_area_California.rds") +
  labs(title = "California")
SD_vs_area_MW <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_SD_vs_area_Midwest.rds") +
  labs(title = "Midwest")

sva_combine <- (SD_vs_area_CA / SD_vs_area_MW) +
  plot_layout(guides = "collect")
sva_combine

# saving:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/Z_final_paper_figures/"
png(filename = paste0(save_dir, Sys.Date(), "_Naive_vs_ensemble_SD_plots_CA_MW_combined.png"),
    height = 10, width = 8, units = "in", res = 600)
sva_combine
dev.off()


# ensemble:naive ratios for CA, MW regions:
ratio_vs_area_CA <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_ratio_vs_area_California.rds") +
  labs(title = "California")
ratio_vs_area_MW <- readRDS("/projectnb/dietzelab/malmborg/EDF/Figures/Plots/2026-05-12_ratio_vs_area_Midwest.rds") +
  labs(title = "Midwest")

ratio_combine <- (ratio_vs_area_CA / ratio_vs_area_MW) +
  plot_layout(guides = "collect")
ratio_combine

# saving:
save_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/Z_final_paper_figures/"
png(filename = paste0(save_dir, Sys.Date(), "_ratio_plots_CA_MW_combined.png"),
    height = 10, width = 8, units = "in", res = 600)
ratio_combine
dev.off()


## Loop for combining the portfolio and COT plots by crop type:
# open correct directories:
port_dir <- "/projectnb/dietzelab/malmborg/EDF/Figures/Portfolio_Plots/"
port_list <- list.files(port_dir)[grepl(".rds", list.files(port_dir)) & grepl("SD_vs", list.files(port_dir))]

# loop for making plots:
port_plots <- list()
for (i in 1:length(port_list)){
  # load plot file name:
  name <- port_list[i] 
  # extract crop type:
  crop <- str_to_title(str_extract(name, "(?<=portfolio_).*?(?=_SD)"))
  # extract location for plot title:
  loc <- str_extract(name, "(?<=12_).*?(?=_portfolio)")
  if(loc == "CA"){
    loc <- "California"
  } else {
    loc <- "Midwest"
  }
  # load plot and change title:
  plot <- readRDS(paste0(port_dir, name)) + labs(title = paste0(loc, " - ", crop))
  # save the plot in the list under the crop name:
  port_list[[crop]] <- plot
}
