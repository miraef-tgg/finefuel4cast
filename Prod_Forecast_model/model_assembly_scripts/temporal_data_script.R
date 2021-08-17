# This script formats spatial data downloaded from GEE and converts the tiff to a csv
# if you change the date range in the GEE script, this will need to change as well
# this takes a long time to run (hours)
# gridmet data is available starting in 1979, ndvi not available til 1986

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <- getActiveDocumentContext()$path
prod_folder<-paste0(dirname(dirname(current_path)))


source(file=paste0("model_assembly_scripts/spatial_data_script.R"))
length(row_keeps_vec)

clim<-stack(paste0("gee_4cast_data/prod_4cast_tiffs/temporal_gee_tiffs.tif"))


clim_df<-as.data.frame(clim)
backup<-clim_df
# View((clim_df[1:100,1:100]))
dim(clim_df)
## ---------------------------------------------- rename GEE data ------------------------------------------------

cur_year <- 2021
cur_month <- 7 #if doing on first week of month, might need to make previous month until data is available on GEE
(length<-3*(cur_year-1979)*12 + 1*12*(cur_year-1986)) 
(length<-length+(3*cur_month + 1*cur_month) )
ncol(clim_df)==length #if dates of download are correct; this will be true

#2020-10, 2020-11, 2020-12, 2021-01 all missing some ndvi data

pr_name_vec<-c(paste0(paste0("pr_",seq(1,12,1)),"_",rep(seq(1979,cur_year,1),each=12)))
pr_name_vec<-pr_name_vec[1:(12*(cur_year-1979)+cur_month)]
tmmx_name_vec<-c(paste0(paste0("tmmx_",seq(1,12,1)),"_",rep(seq(1979,cur_year,1),each=12)))
tmmx_name_vec<-tmmx_name_vec[1:(12*(cur_year-1979)+cur_month)]
vpd_name_vec<-c(paste0(paste0("vpd_",seq(1,12,1)),"_",rep(seq(1979,cur_year,1),each=12)))
vpd_name_vec<-vpd_name_vec[1:(12*(cur_year-1979)+cur_month)]
ndvi_name_vec<-c(paste0(paste0("ndvi_",seq(1,12,1)),"_",rep(seq(1986,cur_year,1),each=12)))
ndvi_name_vec<-ndvi_name_vec[1:(12*(2020-1985)+cur_month)]
name_vec<-c(pr_name_vec, tmmx_name_vec, vpd_name_vec, ndvi_name_vec)

length(name_vec)
names(clim_df)<-name_vec

# check
c_means<-as.data.frame(colMeans(clim_df,na.rm=TRUE))
length(pr_name_vec)*3+(12*(cur_year-1986)-1)==length(name_vec)
c_means$names<-name_vec
# View(c_means)

  
## ---------------------------------------------- clip to row_keeps_vec (mask)------------------------------------------------
clim_df_clip<-clim_df
clim_df_clip$row_names<-seq(1,nrow(clim_df),1)
clim_df_clip<-subset(clim_df_clip,clim_df_clip$row_names %in% row_keeps_vec)

clim_df_clip<-subset(clim_df_clip,select=-c(row_names))

## ---------------------------------------------- clip to row_keeps_vec (mask)------------------------------------------------

write.csv(clim_df_clip, "gee_4cast_data/prod_4cast_csvs/gee_temporal_data.csv", row.names=F)


