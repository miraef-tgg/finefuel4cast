# Code to run Productivity Forecast analyses for Ensley-Field et al. 


## ---------- set-up ----------------

# install and update packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# Set working directory to source file location (this method only works on Rstudio)
current_folder <- dirname(getActiveDocumentContext()$path)
setwd(current_folder)
memory.limit(size=10000000000000000000) #can't increase; some kind of bug (?)

## ---------- assembling data to run models ----------------

# These scripts will download three tiffs from Google Earth Engine with all covariates used in Ensley-Field et al
# The  spatial extent and resolution can be changed; a smaller extent and higher resolution will help this run faster
# see readme for more details
# you will need to move the tiff files produced from your 
# google drive folder to the '~finefuel4cast\Prod_Forecast_model\gee_4cast_data\prod_4cast_tiffs' folder

# RAP_gee_tiffs: https://code.earthengine.google.com/1344640e568dc96d032fecd99d45e4f3
# Spatial_gee_tiffs: https://code.earthengine.google.com/92f1428e7183d07afe5c4d7e42bc374a
# Temporal_gee_tiffs: https://code.earthengine.google.com/02399e2d37e3e6fd92da7fa200038c88
 
 
# script to convert tiff files downloaded from GEE to csvs and formats csv files to include distinguishable date and variable names and columns.

source(file="model_assembly_scripts/spatial_data_script.R")
source(file= "model_assembly_scripts/RAP_data_script.R")
source(file= "model_assembly_scripts/temporal_data_script.R") #this takes a long time (hours) to run
gc() 

# assembles final csv to use in productivity forecast models

source(file= "model_assembly_scripts/march_model_assemble.R")

# check data assembly

#plausible values? correct time range?
mod_dat<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
head(mod_dat)
range(mod_dat$yr)
# 
forecast_dat<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
head(forecast_dat)
range(forecast_dat$yr)

## ---------- running models ----------------

# Runs the forecast, null, ndvi, and climate_ndvi models referenced in paper
source(file= "running_model_scripts/run_all_models.R")
gc()

# Runs the forecast model on all years 1988-2021 and creates point estimates
source(file= "running_model_scripts/prod_cast_point_estimates.R")

# Forecasts 2021 productivity and calculates process and parameter uncertainty from the forecast regression model
# you might need to lower 'nIters' from the default (100) depending on computational limits of your system and the spatial extent you run this over

nIters<-100
source(file= "running_model_scripts/forecast_lm_uncertainty.R")


## ---------- creating figures ----------------

# Creates time series figures or predictive scores comparing four models referenced in manuscript
# time series are saved in 'figures' folder
source(file= "visualization_scripts/model_errors_time_series.R") 


# Creates general four spatial figures in the 'figures/summary_figures' folder
# 'mean_productivity.png' and 'mean_standard_dev.png' are spatial maps of the long term productivity and sd of raw, untransformed herbaceous productivity
# 'mean_residuals_forecast_model.png' and 'correlation_forecast_model.png' produce spatial figures summarize results from the model between 1986 and 2020

source(file= "visualization_scripts/general_vis.R")

# Creates spatial spatial snapshots of residuals from 1987-2020 of the predict_lm model in the 'figures/model_residuals' folder
# has commented out code for variograms
source(file= "visualization_scripts/spatial_residuals_by_model.R") 

