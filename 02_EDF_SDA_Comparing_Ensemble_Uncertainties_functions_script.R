#### EDF PROJECT functions for calculating uncertainties across spatial aggregates ####

## Prepare workspace ##
# load libraries:
library(sf)
library(terra)
#terraOptions(thread = 16)
library(dplyr)


## 1. Function for processing each tif file
# inputs:
#'@param dir = Character vector: file where tiffs can be found
#'@param var = character: variable of interest for each analysis; e.g. soc
#'@param year = numeric: data year of ensemble members
#'@param crops = vector: shapefile for croplands to crop rasters
process_ensemble_members <- function(dir, var, year, crops){
  # choose file by year:
  findfile <- paste0(dir, var, as.character(year))
  findtiff <- list.files(findfile)[grep(".tiff", list.files(findfile))]
  
  # load vector for region:
  vec <- crops
  
  # print progress message:
  print("processing ensemble member rasters")
  
  # loop for processing and making list of rasters:
  ens_rast <- list()
  for (i in 1:length(findtiff)){
    print(i)
    tiff <- paste0(findfile, "/", findtiff[i])
    rast <- terra::rast(tiff)
    vec <- terra::project(vec, rast)
    crop <- terra::crop(rast, vec)
    ens_rast[[findtiff[i]]] <- crop
  }
  #ens_rast[["full_map"]] <- terra::rast(paste0(findfile, "/", findtiff[grep("mean", findtiff)]))
  return(ens_rast)
}


## 2a. Function for identifying crop regions:
# inputs:
#'@param rast = example raster for making object for sorting where crops are/aren't
#'@param crops = vector shapefile of croplands
# can be added later when sorting by crop types:
#'@param classes = table of crop classes for identifying croplands
get_crop <- function(rast, crops){
  # load crops as vector and reproject:
  cv <- crops
  cv <- terra::project(cv, rast)
  # crop example raster to croplands:
  crop_rast <- terra::crop(rast, cv) 
  # load crop classes
  #landClass <- classes
  #cv["CF"] = as.factor(cv["MAIN_CROP"])  # this line throws an error that doesn't prevent the code from running but does seem to stop the function
  landRast <- terra::rasterize(cv, crop_rast)#, "MAIN_CROP")
  is_crop <- !is.na(landRast)
  return(is_crop)
}

## 2b. Function for making is_crop object if croplands is a raster object:
# get_is_crop <- function(crops){
#   is_crop <- !is.na(crops)
#   return(is_crop)
# }


## 3. Function for running analyses ##
# inputs for running analyses in chosen location and scale:
#'@param ens_rast List: list of tiff files from processing function
#'@param is_crop Raster: cropland selection raster from is_crop function
#'@param agg_reg shapefile: vector shapefile for aggregation region, e.g. county
naive_C_uncertainty <- function(ens_rast, is_crop, agg_reg){
  # separate mean/std tiffs and ensemble members:
  ens_mean <- ens_rast[[names(ens_rast)[grep("mean", names(ens_rast))]]]
  ens_std <- ens_rast[[names(ens_rast)[grep("std", names(ens_rast))]]]
  
  # Calculate by county:
  #RegORIG <- terra::vect(region)
  RegORIG <- agg_reg  # vector shapefile SpatVector object
  Reg <- terra::project(RegORIG, ens_mean)
  RegVar <- terra::extract(ens_mean, Reg, fun = mean, na.rm = TRUE)
  Reg[["mean"]] <- RegVar$mean
  
  # means for area that is just cropland:
  cropMean <- ens_mean*is_crop
  RegCropMean <- terra::extract(cropMean, Reg, fun = mean, na.rm = TRUE)
  Reg[["cropMean"]] <- RegCropMean$mean
  
  # totals for area that is just cropland:
  RegCropTotMean <- terra::extract(cropMean*100/1000000, Reg, fun = sum, na.rm = TRUE) #Mg/ha -> Tg
  Reg[["cropTot"]] <- RegCropTotMean$mean

  # naive uncertainty:
  cropVar <- is_crop*ens_std^2
  RegCropTotVar = terra::extract(cropVar, Reg, fun = sum, na.rm = TRUE)
  Reg[["crop_Tot_CV"]] <- sqrt(RegCropTotVar[2])*100/1000000/RegCropTotMean$mean*100
  Reg[["crop_Tot_SD"]] <- sqrt(RegCropTotVar[2])*100/1000
  
  # return data for making maps:
  return(Reg)
}


## 4. Function for ensemble uncertainty calculations:
#'@param ens_rast List: list of raster objects from processing function
#'@param n_regions numeric: number of divisions in regions for aggregating, e.g. number of counties
#'@param is_crop Raster: cropland selection raster from is_crop function
#'@param Reg region variable from previous function
ensemble_C_uncertainty <- function(ens_rast, n_regions, is_crop, Reg){
  # separate ensemble members from mean and std rasters:
  ensems <- ens_rast[names(ens_rast)[grep("ensemble", names(ens_rast))]]
  
  # loop for extracting ensemble member sums:
  ne = length(ensems)
  ens_mems <- as.data.frame(matrix(NA, nrow = n_regions, ncol = ne))
  
  # print progress message:
  print("calculating ensemble member uncertainty")
  
  # Calculate sum for different ensembles, then calculate SD:
  for (e in 1:ne) {
    print(e)
    # load ensemble member:
    tiff <- ens_rast[[e]]
    # clip the ensemble member for crops:
    ens_var <- tiff * is_crop
    # extract for region of aggregation:
    ensTotCropVar <- terra::extract(ens_var, Reg, fun = sum, na.rm = TRUE)
    # add to data frame + Tg conversion:
    ens_mems[,e] <- ensTotCropVar[,2]*100/1000000 ## Mg/ha -> Tg
  }
  # calculate sd over dataframe
  Reg[["crop_ensVar_SD"]] <- apply(ens_mems, 1, sd)*1000 #Gg 
  return(Reg)
}


## Function wrapper for outputting vectors for maps and plots
# inputs:
#'@param ens_rast = raster list from process_ensemble_members 
#'@param crops = vector: shapefile for croplands to crop rasters
#'@param agg_reg = vector: shapefile for aggregate region; e.g. county map
#'@param n_regions = numeric: number of aggregate units; e.g. number of counties
carbon_uncertainty_wrapper <- function(ens_rast, crops, agg_reg, n_regions){
  #ens_rast <- process_ensemble_members(dir, var, year, crops)
  is_crop <- get_crop(ens_rast[[1]], crops)
  #is_crop <- get_is_crop(crops)
  Reg <- naive_C_uncertainty(ens_rast, is_crop, agg_reg)
  Reg <- ensemble_C_uncertainty(ens_rast, n_regions, is_crop, Reg)
  # name <- paste0("Reg_", var)
  # vec_list[name] <- Reg
  return(Reg)
}

# Last update: 8/28/2025
