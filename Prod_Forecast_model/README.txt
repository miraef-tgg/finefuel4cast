The ‘Prod_Forecast_Model’ folder NEEDS you to download data from GEE (Google Earth Engine) that is too large to share over Github:

****Downloading data from GEE:
Sign up for a free account here: https://earthengine.google.com/new_signup/ 

-Scripts to download GEE data:
RAP_gee_tiffs: https://code.earthengine.google.com/1344640e568dc96d032fecd99d45e4f3
Spatial_gee_tiffs: https://code.earthengine.google.com/92f1428e7183d07afe5c4d7e42bc374a
Temporal_gee_tiffs: https://code.earthengine.google.com/02399e2d37e3e6fd92da7fa200038c88
(the last GEE script takes >20 minutes to run and export)
These three .tiffs will appear in your google drive after running these scripts. You must then download and transfer them to the '~finefuel4cast\Prod_Forecast_model\gee_4cast_data\prod_4cast_tiffs' folder to run our analysis. 

-quick intro that should allow you to run and make basic edits to our script we recommend: https://www.youtube.com/watch?v=BUo-8I0peuI

-Areas of interest to run this model can be modified by editing the ‘gb_region’ FeatureCollection in the imports within Google Earth Engine, or drawing new polygon and changing the ‘region’ argument in the’ export.image.toDrive()’ function. 
-We reduced the resolution from 4000m to 10000m in hopes that this analysis can be run on most computers. You can increase and reduce the resolution by editing the ‘scale’ argument in the ‘export.image.toDrive()’ function. 
-This won’t work if you change the spatial extent outside the Intermountain West, where the datasets we download are available.
-The extent and resolution included as the default in this folder run on our computer, we are using a windows 10 workstation with an i7-7700 Intel Core Processor, CPU @3.6GHz, NVIDIA Quadro P600, 16 GB of RAM @2400MHz, and an SSD. 
-Reducing the spatial extent and increasing the spatial resolution will help this run faster and use fewer computational resources.

Once you've downloaded the data; this folder contains code that:

-Formats soil (spatial), weather and ndvi (temporal), productivity and cover (RAP data) downloaded from GEE over the desired spatial extent. 
-Runs the four models described in Ensley-Field et al to create a forecast model and comparison models
-Creates point estimates of hindcasts from 1988-2020 and a forecast for 2021 from the forecast model. It saves all of these together in one csv file.
-Separately runs a 2021 forecast and calculates process and parameter uncertainty using monte carlo sampling. We ran it with 500 iterations, this might be computationally prohibitive and you can lower this number.
-Creates figures showing maps of long term mean and standard deviation of the downloaded productivity data. It then creates a map of the long term mean of residuals and correlation between productivity data and the model run.
-Creates figures of spatial residuals by model 
-All scripts used can be run in the “run_everything.R” script (once the 'Fuels_Model has been run and data from GEE is downloaded)
