# This script creates a forecast for the null, forecast, clim, and clim_ndvi models using data from years 1987-2021

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


model_df_orig<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
model_df<-model_df_orig

# ---------------------------------------------- checking/cleaning ------------------------------------------------


#mod1: linear forecast
mod1_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                 prev_pfg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm))



#mod2: linear null/spatial only
mod2_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr,
                                 prev_pfg_frac,z_ssm,z_bulk_dens))


#mod3: clim  everything
mod3_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                 prev_pfg_frac,prev_z_agb_prev_afg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm,
                                 z_pr_april,z_pr_may, z_pr_june ,
                                 z_pr_july,z_pr_aug, z_pr_sep ,
                                 z_tmmx_april,z_tmmx_may, z_tmmx_june ,
                                 z_tmmx_july,z_tmmx_aug, z_tmmx_sep ,
                                 z_vpd_april,z_vpd_may, z_vpd_june,
                                 z_vpd_july,z_vpd_aug, z_vpd_sep
                                 ))


#mod4: clim and ndvi
mod4_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                      prev_pfg_frac,prev_z_agb_prev_afg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm,
                                      z_pr_april,z_pr_may, z_pr_june ,
                                      z_pr_july,z_pr_aug, z_pr_sep ,
                                      z_tmmx_april,z_tmmx_may, z_tmmx_june ,
                                      z_tmmx_july,z_tmmx_aug, z_tmmx_sep ,
                                      z_vpd_april,z_vpd_may, z_vpd_june,
                                      z_vpd_july,z_vpd_aug, z_vpd_sep ,
                                      z_ndvi_april,z_ndvi_may, z_ndvi_june,
                                      z_ndvi_july,z_ndvi_aug, z_ndvi_sep

                                       ))
# mod5: 3 month out binary noaa forecast
# not running this model--we looked into weather we should try to include forecasted weather data in our forecast, and decided against it
# mod5_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
#                                       prev_pfg_frac,prev_z_agb_prev_afg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm,
#                                       forecast_pr_april,forecast_pr_may, forecast_pr_june ,
#                                       forecast_pr_july,forecast_pr_aug, forecast_pr_sep ,
#                                       forecast_tmmx_april,forecast_tmmx_may, forecast_tmmx_june 
#                                       forecast_tmmx_july,forecast_tmmx_aug, forecast_tmmx_sep
#                                       
# ))

(nLocs<-nrow(mod1_data)/34)

coord_df<-as.data.frame(cbind(model_df$long, model_df$lat))[1:nLocs,]
names(coord_df)<-c("long", "lat")

## various models and save locs
mod_data_list<-list(mod1_data,mod2_data,mod3_data,mod4_data)

save_loc1<-"/figures/march_vis/forecast/"
save_loc2<-"/figures/march_vis/spatial_only_model/"
save_loc3<-"/figures/march_vis/climate/"
save_loc4<-"/figures/march_vis/climate_and_ndvi/"
save_loc_list<-list(save_loc1,save_loc2,save_loc3,save_loc4)


save_name1<-"march_forecast_lm"
save_name2<-"march_spatial_only_lm"
save_name3<-"march_climate_lm"
save_name4<-"march_climate_and_ndvi_lm"
save_name_list<-list(save_name1,save_name2,save_name3,save_name4)

# -------------- Loop ----------------

for ( i in 1:4){
  mod_data<-mod_data_list[[i]]
  save_loc<-save_loc_list[[i]]
  save_name<-save_name_list[[i]]

X<-as.matrix(subset(mod_data, select=-c(z_agb,yr)))
Y<-mod_data[,1]
lm_data<-as.data.frame(cbind(Y,X))
names(lm_data)<-c("z_agb", names(mod_data)[!names(mod_data) %in% c("yr","z_agb")])
mod_fit<-lm(z_agb~.,data=lm_data) 


#coeffs
names_coefs<-c("int",  names(lm_data))
names_coefs<-names_coefs[!(names_coefs=="z_agb" | names_coefs=="yr" )]
lm_coeffs<-as.data.frame(cbind(names_coefs,as.vector(coef(mod_fit)) ))
# View(lm_coeffs)
write.csv(lm_coeffs,paste0("prod_model_outputs/", save_name_list[[i]], "_coeffs.csv"), row.names = FALSE)

# making wide format predictions
lm_preds<-mod_fit$fitted.values
lm_resids<-mod_fit$residuals

# check
lm_resids[1:10]-(lm_data$agb[1:10]-mod_fit$fitted.values[1:10])
lm_preds_m<-matrix(as.vector(lm_preds),byrow=FALSE,nrow=(length(lm_preds))/34,ncol=34)
lm_preds_m<-as.data.frame(lm_preds_m)
names(lm_preds_m)<-paste0("lm_preds", seq(1987,2020))


# wide format residuals, data, predictions
resid_m<-matrix(NA,nrow=nLocs,ncol=34)
agb_m<-matrix(NA,nrow=nLocs,ncol=34)
Ypred_m<-matrix(NA,nrow=nLocs,ncol=34)

i<-1;j<-1
for ( r in 1:length(lm_data$z_agb)){
  agb_m[i,j]<-lm_data$z_agb[r]
  Ypred_m[i,j]<-mod_fit$fitted.values[r]
  resid_m[i,j]<-mod_fit$fitted.values[r]-lm_data$z_agb[r]
  i<-i+1
  if (i>nLocs){
    j=j+1
    i=1
  }
}

prod_preds<-list(Ypred_m, agb_m,resid_m,  coord_df, mod_fit)
names(prod_preds)<-c("prod_mean", "agb_m","resids", "coord_df", "mod_fit")

print(summary(mod_fit))
saveRDS(prod_preds, paste0("prod_model_outputs/", save_name,".rds"))

}


