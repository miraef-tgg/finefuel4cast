# Code to run Productivity Forecast analyses for Ensley-Field et al. 


## ---------- set-up ----------------

# install and update packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# Set working directory to source file location (this method only works on Rstudio)
current_folder <- dirname(getActiveDocumentContext()$path)
setwd(current_folder)
memory.limit(size=100000) #minimum needed to run default

## ---------- assembling data to run models ----------------

# you only need to run these if you wish to run this model on a different spatial extent/resolution than the default provided
# different spatial extents and resolutions can be downloaded using the following links:
# RAP_gee_tiffs: https://code.earthengine.google.com/59e173e815628e7d4456a0fe923b0f45
# Spatial_gee_tiffs: https://code.earthengine.google.com/8cdcbddfe310a0ed1a45e3cdde301c4e
# Temporal_gee_tiffs: https://code.earthengine.google.com/85d90e7e2b994e26a3cfe0b1d810d34e
 
 
# script to convert tiff files downloaded from GEE to csvs and formats csv files to include distinguishable date and variable names and columns.

# source(file="model_assembly_scripts/spatial_data_script.R")
# source(file= "model_assembly_scripts/RAP_data_script.R")
# source(file= "model_assembly_scripts/temporal_data_script.R") #this takes a long time (hours) to run
 
# assembles final csv to use in productivity forecast models

# source(file= "model_assembly_scripts/march_model_assemble.R")

# check data assembly

#plausible values? correct time range?
# mod_dat<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
# head(mod_dat)
# range(mod_dat$yr)
# 
# forecast_dat<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
# head(forecast_dat)
# range(forecast_dat$yr)

## ---------- running models ----------------

# Runs the forecast, null, ndvi, and climate_ndvi models referenced in paper
source(file= "running_model_scripts/run_all_models.R")

# Forecasts 2021 productivity and calculates process and parameter uncertainty from the forecast regression model
# you might need to lower 'nIters' from the default (500) depending on computational limits of your system and the spatial extent you run this over

nIters<-100
source(file= "running_model_scripts/forecast_lm_uncertainty.R")

# Creates hindcasts of 1986-2020 RAP data using data available in early march
# you might need to lower 'nIters'from the default (500) depending on computational limits of your system and the spatial extent you run this over

nIters<-100
source(file= "running_model_scripts/hindcast_lm_uncertainty.R") #takes a long time (>30 minutes) to run


## ---------- creating figures ----------------

# Creates time series figures or predictive scores comparing four models referenced in manuscript
# time series are saved in 'figures' folder
source(file= "visualization_scripts/model_errors_time_series.R") 


# Creates general four spatial figures in the 'figures/summary_figures' folder
# 'mean_productivity.png' and 'mean_standard_dev.png' are spatial maps of the long term productivity and sd of raw, untransformed herbaceous productivity
# 'mean_residuals_forecast_model.png' and 'correlation_forecast_model.png' produce spatial figures summarize results from the model between 1986 and 2020

source(file= "visualization_scripts/general_vis.R")

# Creates spatial spatial snapshots of residuals from 1987-2020 of all models in the 'figures/
# has commented out code for variograms
source(file= "visualization_scripts/spatial_residuals_by_model.R") 

