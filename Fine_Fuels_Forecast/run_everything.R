# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path
BL_folder<-dirname(dirname((current_path)))

current_folder <- dirname(getActiveDocumentContext()$path)
setwd(current_folder)
setwd("..")
getwd()

#run the spin up, save outputs
source(file = "Fine_Fuels_Forecast/predict_latent_fine_fuel.R")

# make spatial map 2021
source(file = "Fine_Fuels_Forecast/forecast_2021_map.R")

# make latent fine fuel time series figures 1998-2021
source(file = "Fine_Fuels_Forecast/time_series_by_district.R")

# error partitioning
source(file = "Fine_Fuels_Forecast/partition_error.R")
