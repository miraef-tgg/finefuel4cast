# this script creates point estimates for a forecast of 2021 as well as hindcasts of 1988-2021
# saves results in a .csv file

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df_orig<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
model_df<-model_df_orig

model_df_orig2021<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
model_df2021<-model_df_orig2021

mod1_data2021<-subset(model_df2021, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                             prev_pfg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm))

(lm_coeffs<-read.csv( "prod_model_outputs/march_forecast_lm_coeffs.csv"))

nLocs<-nrow(mod1_data)

# # ----------------------- Forecast point estimates (2021)  -------------------------
#use lm_coeffs for point estimates of coefficients
lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])
march_forecast_point_est2021<-matrix(NA,nrow=nIters,ncol=nLocs)

march_forecast_point_est2021<-lm_coeffs[lm_coeffs[,1]=="int",2]+
  lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*mod1_data$prev_z_agb +
  lm_coeffs[lm_coeffs[,1]=="z_pr",2]*mod1_data$z_pr +
  lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*mod1_data$z_tmmx +
  lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*mod1_data$z_vpd +
  lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*mod1_data$z_ndvi +
  lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*mod1_data$pr_frac +
  lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*mod1_data$prev_pfg_frac +
  lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*mod1_data$z_ssm +
  lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*mod1_data$z_bulk_dens +
  lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*mod1_data$z_pr_z_ssm 

names(march_forecast_point_est)<-"forecast2021"
# write.csv(march_forecast_point_est,"prod_model_outputs/forecast2021/march_forecast_point_est_2021.csv",row.names = F )


# # ----------------------- Hindcasts point estimates (1988-2020) -------------------------


length(seq(1988,2020,1))
(nLocs<-nrow(model_df)/34)
hindcast_point_est = matrix(NA,nrow=nLocs,ncol=33)

count=1
for ( cur_yr in 1988:2020){
  
  prev_yr=cur_yr-1
  cur_yr_df<-subset(model_df, model_df$yr==prev_yr)
  
  #use lm_coeffs for point estimates of coefficients
  lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])
  march_forecast_point_est<-matrix(NA,nrow=1,ncol=nLocs)
  
  march_forecast_point_est<-lm_coeffs[lm_coeffs[,1]=="int",2]+
    lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*cur_yr_df$prev_z_agb +
    lm_coeffs[lm_coeffs[,1]=="z_pr",2]*cur_yr_df$z_pr +
    lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*cur_yr_df$z_tmmx +
    lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*cur_yr_df$z_vpd +
    lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*cur_yr_df$z_ndvi +
    lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*cur_yr_df$pr_frac +
    lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*cur_yr_df$prev_pfg_frac +
    lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*cur_yr_df$z_ssm +
    lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*cur_yr_df$z_bulk_dens +
    lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*cur_yr_df$z_pr_z_ssm 
  
  hindcast_point_est[,count]=march_forecast_point_est
  count=count+1
}

hindcast_point_est<-as.data.frame(hindcast_point_est)
names(hindcast_point_est)<-paste0(rep("hindcast",33), seq(1988,2020,1))
# write.csv(hindcast_point_est,"prod_model_outputs/forecast2021/hindcast_point_est_1988_2020.csv",row.names = F )



all_casts<-cbind(hindcast_point_est, march_forecast_point_est)
write.csv(all_casts,"prod_model_outputs/forecast2021/prod_cast_1988_2021.csv",row.names = F )
