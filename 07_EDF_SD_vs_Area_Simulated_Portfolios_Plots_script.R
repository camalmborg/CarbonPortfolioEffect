### SD vs Area plots for EDF project - Simulated Portfolio Runs

library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpmisc)

# variable names for plots:
var_names <- c("AGB", "SOC", "LAI", "SMF")
# set name for labeling plots:
plot_var_name <- var_names[2]

## Function for preparing plot data:
#'@param portfolio_list = list for pixel group portfolio with SD calculations
#'@param group = which pixel sample group 
prepare_portfolio_data <- function(portfolio_list, group){
  # select portfolio list group by number of pixels:
  port <- portfolio_list[[group]]
  # add a rep number:
  for (i in 1:length(port)){
    port[[i]]$member <- paste0("Rep_", i)
    #port[[i]]$ID <- paste0("Rep", i, "_", seq_len(nrow(port[[i]])))
  }
  # make into one vector:
  rep_vec <- do.call(rbind, port)

  # calculate the delta between naive and ensemble calculations:
  rep_vec$delta <- rep_vec$crop_ensVar_SD - rep_vec$crop_Tot_SD
  # calculate ratio between naive and ensemble calculations:
  rep_vec$ratio <- rep_vec$crop_Tot_SD / rep_vec$crop_ensVar_SD
  # other direction ratio:
  rep_vec$ratio_rev <- rep_vec$crop_ensVar_SD / rep_vec$crop_Tot_SD
  return(rep_vec)
}

## Make a data frame for plots:
compile_portfolio_df <- function(portfolio_list, pixel_groups){
  # make list of each portfolio data:
  portfolio_df_list <- list()
  for(i in 1:length(pixel_groups)){
    portfolio_df_list[[i]] <- prepare_portfolio_data(portfolio_list, pixel_groups[i])
  }
  # make into one data frame:
  portfolio_df <- do.call(rbind, portfolio_df_list)
  return(portfolio_df)
}

# vec <- compile_portfolio_df(portfolio_list,
#                             pixel_groups)


### Plot crop area by county vs SD:
SD_vs_area_plot <- function(portfolio_list, pixel_groups) {
  # compile data:
  vec <- compile_portfolio_df(portfolio_list,
                            pixel_groups)
  # coerce to data.frame for plot:
  plot_data <- as.data.frame(vec) %>%
    # select columns:
    select(c(group, agg_n, mean_area_m2, crop_Tot_SD, crop_ensVar_SD, delta, ratio, ratio_rev)) %>%
    # pivot longer:
    pivot_longer(
      cols = c(crop_Tot_SD, crop_ensVar_SD),
      names_to = "variable",
      values_to = "value"
    ) %>%
    arrange(factor(group))
  
  # # color palette:
  # plot_palette <- c("orchid4", "chocolate3")
  
  SD_vs_area <- ggplot(plot_data, aes(x = agg_n, y = value)) +
    geom_point(aes(color = variable, fill = variable), size = 2.5) +
    geom_smooth(aes(group = variable, fill = variable, color = variable), method = "lm", formula = y ~ x , se = TRUE, linewidth = 0.5, alpha = 0.15) +
    stat_poly_eq(
      aes(label = paste(..eq.label.., "*\",  \"*", ..rr.label.., sep = ""),
          color = variable,
          group = variable),
      formula = y ~ x,
      parse = TRUE,
      size = 5) +
    ggtitle(paste0("Naive vs. Ensemble SD calculations: ", plot_loc)) +
    labs(x = "Number of 1km Pixels",
         y = "SD", 
         color = "Calculation Method",
         fill = "Calculation Method") +
    scale_color_discrete(labels = c("crop_Tot_SD" = "Naive", 
                                    "crop_ensVar_SD" = "Ensemble")) +
    scale_fill_discrete(labels = c("crop_Tot_SD" = "Naive", 
                                   "crop_ensVar_SD" = "Ensemble")) +
    scale_x_log10() +
    scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) +
    theme_bw()
  
  # return:
  return(SD_vs_area)
}
# view:
#SD_vs_area

delta_vs_area_plot <- function(portfolio_list, pixel_groups){
  # compile data:
  vec <- compile_portfolio_df(portfolio_list,
                              pixel_groups)
  # coerce to data.frame for plot:
  plot_data <- as.data.frame(vec) %>%
    # select columns:
    select(c(group, agg_n, mean_area_m2, crop_Tot_SD, crop_ensVar_SD, delta, ratio, ratio_rev)) %>%
    # pivot longer:
    pivot_longer(
      cols = c(crop_Tot_SD, crop_ensVar_SD),
      names_to = "variable",
      values_to = "value"
    ) %>%
    arrange(factor(group))
  
  ## Plot
  delta_vs_area <- ggplot(plot_data, aes(x = agg_n, y = delta)) +
    geom_point(size = 2.5, color = "navy") +
    geom_smooth(method = "lm", se = TRUE, color = "navy", linewidth = 0.5, alpha = 0.15) +
    stat_poly_eq(
      aes(label = paste(..eq.label.., "*\",  \"*", ..rr.label.., sep = "")),
      color = "navy",
      formula = y ~ x,
      parse = TRUE,
      size = 5) +
    geom_point(size = 1.25, color = "navy") +
    geom_smooth(method = "lm", se = TRUE, color = "navy", linewidth = 0.5, alpha = 0.15) +
    ggtitle(paste0("Naive - Ensemble SD calculations: ", plot_loc)) +
    labs(x = "Number of 1km Pixels",
         y = "Ensemble SD - Naive SD") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    theme(legend.position = "none")
  
  return(delta_vs_area)
}


ratio_vs_area_plot <- function(portfolio_list, pixel_groups){
  # compile data:
  vec <- compile_portfolio_df(portfolio_list,
                              pixel_groups)
  # coerce to data.frame for plot:
  plot_data <- as.data.frame(vec) %>%
    # select columns:
    select(c(group, agg_n, mean_area_m2, crop_Tot_SD, crop_ensVar_SD, delta, ratio, ratio_rev)) %>%
    # pivot longer:
    pivot_longer(
      cols = c(crop_Tot_SD, crop_ensVar_SD),
      names_to = "variable",
      values_to = "value"
    ) %>%
    arrange(factor(group))
  
  ## Plot
  ratio_vs_area <- ggplot(plot_data, aes(x = agg_n, y = ratio_rev)) +
    geom_point(size = 2.5, color = "navy") +  
    geom_smooth(aes(group = variable), method = "lm", se = TRUE, color = "navy", linewidth = 0.5, alpha = 0.15) +
    stat_poly_eq(
      aes(label = paste(..eq.label.., "*\",  \"*", ..rr.label.., sep = "")),
      color = "navy",
      formula = y ~ x,
      parse = TRUE,
      size = 5) +
    ggtitle(paste0("Ensemble:Naive (Ratio Plot) - ", plot_loc)) +
    labs(x = "Log(Number of 1km pixels)",
         y = "Log(Ratio of Ensemble SD : Naive SD)") +
    scale_x_log10() +
    scale_y_log10(breaks = c(0, 1, 5, 10, 25, 50, 100)) +
    theme_bw() +
    theme(legend.position = "none")
  
  return(ratio_vs_area)
}


#ratio_vs_area



# which crop group (change based on crops):
#crop_portfolio = citrus_portfolios
# portfolio names for list processing:
#pixel_groups <- names(crop_portfolio)

# for testing:
#portfolio_list <- crop_portfolio
#group = pixel_groups[3]
