# Make portfolio plots and save:

# run the functions script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/01_EDF_SDA_Portfolio_Sampling_functions_script.R")

# run the California portfolios script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/06_EDF_Simulated_Portfolios_California.R")

# run the plotting script:
source("/projectnb/dietzelab/malmborg/EDF_C_Portfolio_Project/07_EDF_SD_vs_Area_Simulated_Portfolios_script.R")

## Get all the California portfolios:
n_pixels_citrus = c(1, 10, 100, 1000)
n_pixels = c(1, 10, 100, 1000, 10000)

## Get crop portfolio for each run
# task id from cluster run:
task_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# portfolio:
crop_portfolio <- crop_group_list[[task_id]]
# name of group:
portfolio_name <- names(crop_group_list)[task_id]
# get n_pixels:
if(portfolio_name == "citrus"){
  n_pixels = n_pixels_citrus
} else {
  n_pixels = n_pixels
}

## Run portfolio
crop_portfolios <- all_portfolios_runs(crop_group = crop_portfolio,
                                         ens_rast = ens_rast,
                                         n_pixels_vec = n_pixels,
                                         n_reps = 2)

## Run plot
# pixel group names:
pixel_groups <- names(crop_portfolios)
# compile portfolio data for plots:
portfolio_df <- compile_portfolio_df(portfolio_list = crop_portfolios,
                                     pixel_groups = pixel_groups)


