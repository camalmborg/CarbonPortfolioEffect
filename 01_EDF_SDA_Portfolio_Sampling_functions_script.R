#### EDF PROJECT functions for portfolio sampling ####

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
library(dplyr)

## 1. Function for random sampling
#'@param crop_group = subset vector of specific crop type
#'@param ens_rast = ens_rast object from SDA spatial aggregates code runs
#'@param n_pixels = number of pixels for sample portfolio
portfolio_sampler <- function(crop_group, ens_rast, n_pixels){
  # convert crops back to polygon:
  crop_type <- vect(crop_group)
  # project to raster crs:
  crop_type <- project(crop_type, ens_rast[[1]])
  # crop raster to crop area:
  crop_rast <- crop(ens_rast[[1]], crop_type)
  # mask to crop_type area:
  crop_mask <- mask(crop_rast, crop_type)
  # put into meters-based projection:
  crop_mask <- project(crop_mask, "EPSG:3857")
  
  # sample the raster of crop type:
  portfolio_sample <- spatSample(crop_mask,
                                 size = n_pixels,
                                 method = "random",
                                 as.points = TRUE,
                                 #values = TRUE,
                                 na.rm = TRUE)
  # crop the original raster to the portfolio sample:
  portfolio_crop <- crop(crop_mask, portfolio_sample)
  # mask to portfolio:
  portfolio_mask <- mask(portfolio_crop, portfolio_sample)
  return(portfolio_mask)
}