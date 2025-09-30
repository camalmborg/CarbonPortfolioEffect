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
  # convert crops back to terra polygon:
  crop_type <- vect(crop_group)
  # project to raster crs:
  crop_type <- project(crop_type, ens_rast[[1]])
  # crop raster to crop area:
  crop_rast <- crop(ens_rast[[1]], crop_type)
  # mask to crop_type area:
  crop_mask <- mask(crop_rast, crop_type)
  # put into meters-based, equal-area projection:
  #crop_mask <- project(crop_mask, "EPSG:3857")
  crop_mask <- project(crop_mask, "EPSG:9822")
  
  # make km resolution
  # template raster with desired resolution:
  template <- rast(ext(crop_mask), resolution = 1000, crs = crs(crop_mask))
  # resample:
  resample <- resample(crop_mask, template, method = "bilinear")
  
  # sample raster of crop type
  # get cells with values:
  cells <- which(!is.na(values(resample)))
  # take random sample:
  sample <- sample(cells, n_pixels, replace = FALSE)
  # get raster values for sample cells
  values <- values(resample)[sample]
  # make an empty raster to fill with sampled values:
  new_rast <- resample
  values(new_rast) <- NA
  new_rast[sample] <- values
  
  # make into polygon:
  portfolio_poly <- as.polygons(new_rast, dissolve = FALSE)
  # disaggregate to separate:
  portfolio_poly <- disagg(portfolio_poly)
  # add expanse of region column:
  portfolio_poly[["area_m2"]] <- expanse(portfolio_poly, unit = "m")
  # add name to portfolio:
  portfolio_poly[["group"]] <- paste0(as.character(n_pixels), "_pixel_group")
  # aggregate into one portfolio:
  portfolio_poly <- aggregate(portfolio_poly, by = "group")
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
  
## 3. Function for ensemble uncertainty calculations:
#'@param ens_rast List: list of raster objects from processing function
#'@param n_regions numeric: number of rows in portfolio_naive object
#'@param Reg region variable from previous function
portfolio_ens <- function(ens_rast, n_pixels, Reg){
  # separate ensemble members from mean and std rasters:
  ensems <- ens_rast[names(ens_rast)[grep("ensemble", names(ens_rast))]]
  
  # loop for extracting ensemble member sums:
  ne = length(ensems)
  ens_mems <- as.data.frame(matrix(NA, nrow = n_pixels, ncol = ne))
  
  # print progress message:
  #print("calculating ensemble member uncertainty")
  
  # Calculate sum for different ensembles, then calculate SD:
  for (e in 1:ne) {
    #print(e)
    # load ensemble member:
    ens_var <- ens_rast[[e]]
    # extract for region of aggregation:
    ensTotCropVar <- terra::extract(ens_var, Reg, fun = sum, na.rm = TRUE)
    # add to data frame + Tg conversion:
    ens_mems[,e] <- ensTotCropVar[,2]
  }
  # calculate sd over dataframe
  Reg[["crop_ensVar_SD"]] <- apply(ens_mems, 1, sd)
  return(Reg)
}

## Function wrapper for outputting vectors for maps and plots
# inputs:
#'@param ens_rast = raster list from process_ensemble_members 
#'@param portfolio = vector: shapefile for croplands to crop rasters
#'@param agg_reg = vector: shapefile for aggregate region; e.g. county map
#'@param n_regions = numeric: number of aggregate units; e.g. number of counties
portfolio_naive_ens_wrapper <- function(crop_group, ens_rast, n_pixels){
  portfolio <- portfolio_sampler(crop_group, ens_rast, n_pixels)
  Reg <- portfolio_naive(ens_rast, portfolio)
  Reg <- portfolio_ens(ens_rast, n_regions = nrow(Reg), Reg)
  return(Reg)
}

## Function to run multiple times in a row
# inputs:
#'@param crop_group = subset vector of specific crop type
#'@param ens_rast = raster list from process_ensemble_members 
#'@param n_pixels = number of pixels for sample portfolio
#'@param n_reps = number of times to do the sample
portfolio_run <- function(crop_group, ens_rast, n_pixels, n_reps){
  portfolio <- list()
  for (i in 1:n_reps){
    # progress:
    #print(i)
    # add portfolio to list:
    portfolio[[i]] <- portfolio_naive_ens_wrapper(crop_group, ens_rast, n_pixels)
  }
  #portfolio_bind <- do.call(rbind, portfolio)
  return(portfolio)
}

## Function to get full portfolio runs for single crop group:
#'@param crop_group = subset vector of specific crop type
#'@param ens_rast = raster list from process_ensemble_members 
#'@param n_pixels_vec = numeric vector of pixel numbers for different portfolios
#'@param n_reps = number of times to sample each crop_group
all_portfolios_runs <- function(crop_group, ens_rast, n_pixels_vec, n_reps){
  full_portfolio_list <- list()
  for(i in 1:length(n_pixels_vec)){
    # list member name:
    p_name <- paste0(as.character(n_pixels_vec[i]), "_pixel_portfolio")
    # progress:
    print(p_name)
    # fill list:
    full_portfolio_list[[p_name]] <- portfolio_run(crop_group = crop_group,
                                                     ens_rast = ens_rast,
                                                     n_pixels = n_pixels_vec[i],
                                                     n_reps = n_reps)
    # add a portfolio column:
    # full_portfolio_list[[p_name]]$portfolio_group <- p_name
  }
  return(full_portfolio_list)
}

### ARCHIVE ###

## Note: spatSample method = "random" may place points in the same pixel, had to troubleshoot
# # sample the raster of crop type:
# portfolio_sample <- spatSample(crop_mask,
#                                size = n_pixels,
#                                method = "random",
#                                as.points = TRUE,
#                                #values = TRUE,
#                                na.rm = TRUE)