# This script formats Rangeland Analysis Product data downloaded from GEE and converts the tiff to a csv
# downloads percent cover in addition to biomass data

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <- getActiveDocumentContext()$path
prod_folder<-paste0(dirname(dirname(current_path)))

source(file="model_assembly_scripts/spatial_data_script.R")
length(row_keeps_vec)

ts<-stack("gee_4cast_data/prod_4cast_tiffs/RAP_gee_tiffs.tif")

ts_df<-as.data.frame(ts)
# View((ts_df[1:100,]))
dim(ts_df)
## ---------------------------------------------- rename GEE data ------------------------------------------------

data_names<-c("percent_afg","percent_pfg","percent_shr","afgAGB","pfgAGB" )

name_vec<-vector()
count<-1
for (yr in 1986:2020){
  name_vec[count:(count+4)]<-paste0(data_names, "_", yr)
  count=count+5
}
length(name_vec)
names(ts_df)<-name_vec

## ---------------------------------------------- clip to row_keeps_vec (mask)------------------------------------------------
ts_df_clip<-ts_df
ts_df_clip$row_names<-seq(1,nrow(ts_df),1)
ts_df_clip<-subset(ts_df_clip,ts_df_clip$row_names %in% row_keeps_vec)

ts_df_clip<-subset(ts_df_clip,select=-c(row_names))
dim(ts_df_clip)
## ---------------------------------------------- clip to row_keeps_vec (mask)------------------------------------------------
# 
write.csv(ts_df_clip, "gee_4cast_data/prod_4cast_csvs/gee_RAP_data.csv", row.names=F)

          