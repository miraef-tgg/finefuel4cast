# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path
BL_folder<-dirname(dirname((current_path)))

current_folder <- dirname(getActiveDocumentContext()$path)
setwd(current_folder)
setwd("..")


source(file = "Fine_Fuels_Forecast/assigning_districts.R")

#run 10 years of spin up on real data, runs 11th year on forecasted or hindcasted data, save outputs
source(file = "Fine_Fuels_Forecast/predict_latent_fine_fuel.R")

# make spatial map for latent fine fuels of 2021
source(file = "Fine_Fuels_Forecast/forecast_2021_map.R")
#look
# browseURL("Prod_Forecast_model/Figures/forecast2021.png")

# make latent fine fuel time series figures 1998-2021
source(file = "Fine_Fuels_Forecast/time_series_by_district.R") 
#look
# browseURL("Fine_Fuels_Forecast/Figures/time_series_districts.png")

# error partitioning
source(file = "Fine_Fuels_Forecast/partition_error.R")
#look
# browseURL("Fine_Fuels_Forecast/Figures/error_by_source.png")

