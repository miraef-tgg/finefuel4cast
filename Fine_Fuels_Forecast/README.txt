The ‘Fine_Fuels_Forecast’ folder contains code that:

- It assigns the BLM district to the spatial extent downloaded in the 'Prod_Forecast_Model'
- Creates a hindcast of latent fine fuel loads from 1998-2020 and a forecast for 2021 by running through 10 years of the Fuels Model (Eq 2 in Ensley-Field et al) 
- It uses actual productivity data for the first ten years, and then runs the 11th year on predicted productivity data and saves the output
- Creates a spatial map of latent fine fuels of 2021
- Creates time series figures by district
- Partitions error by doing creating a forecast for 100 locations for the year 2021 with 8 different combinations of uncertainty
- All scripts used can be run in the “run_everything.R” script (once the 'Fuels_Model and 'Prod_Forecast_Model' have been run)

inputs:
-Prod_Forecast_model/prod_model_outputs/forecast2021/prod_cast_1988_2021.csv
-Prod_Forecast_model/prod_model_outputs/march_forecast_lm.rds
-Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_param_2021.csv
-Prod_Forecast_model/gee_4cast_data/gis_data/states.shp
-Prod_Forecast_model/gee_4cast_data/gis_data/admin.shp
-Prod_Forecast_model/prod_model_outputs/loc_keeps.csv
-Fuels_model/model_outputs/fuels_model.rds

outputs:
Figures folder: 
-time_series_districts.png
-error_by_source.png
-forecast2021.png
-forecast2021_legend.png

output_data folder:
-latent_forecast_distrib.csv
-latent_fuel_perc_sp.rds
