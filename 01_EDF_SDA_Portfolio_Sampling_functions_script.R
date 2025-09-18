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
  # make a vector:
  portfolio_poly <- as.polygons(portfolio_mask)
  # turn into individual polygons
  portfolio_poly <- disagg(portfolio_poly)
  # project to ens_rast:
  portfolio_poly <- project(portfolio_poly, ens_rast[[1]])
  # return the portfolio:
  return(portfolio_poly)
}

## 2. Function for naive uncertainty for portfolio:
#'@param ens_rast = ens_rast object of processed SDA outputs
#'@param portfolio_sample = portfolio vector object created with portfolio_sampler function
portfolio_naive <- function(ens_rast, portfolio_sample){
  # separate mean/std tiffs and ensemble members:
  ens_mean <- ens_rast[[names(ens_rast)[grep("mean", names(ens_rast))]]]
  ens_std <- ens_rast[[names(ens_rast)[grep("std", names(ens_rast))]]]
  
  # Calculate by aggregating region:
  RegORIG <- portfolio_sample
  Reg <- terra::project(RegORIG, ens_mean)
  
  # means for portfolio:
  RegCropMean <- terra::extract(ens_mean, Reg, fun = mean, na.rm = TRUE)
  Reg[["cropMean"]] <- RegCropMean$mean
  
  # totals for area that is just cropland:
  RegCropTotMean <- terra::extract(ens_mean, Reg, fun = sum, na.rm = TRUE) 
  Reg[["cropTot"]] <- RegCropTotMean$mean
  
  # naive uncertainty:
  cropVar <- ens_std^2
  RegCropTotVar = terra::extract(cropVar, Reg, fun = sum, na.rm = TRUE)
  Reg[["crop_Tot_CV"]] <- sqrt(RegCropTotVar[2])/RegCropTotMean$mean
  Reg[["crop_Tot_SD"]] <- sqrt(RegCropTotVar[2])
  
  # return portfolio vector with naive calc:
  return(Reg)
}
  
