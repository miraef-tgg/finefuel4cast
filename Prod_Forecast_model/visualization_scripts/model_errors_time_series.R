#loading data and pcks
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, gstat,stringr,raster,rstudioapi, rgdal, spacetime,ape,FRK,glmnet,install = TRUE, update = getOption("pac_update"), character.only = FALSE)

## ---------------------------------------- MAPE,MSPE,MBIAS ---------------------------------
options(scipen = 999)
space_only_predictions<-readRDS( "prod_model_outputs/march_spatial_only_lm.rds")
predictions_lm<-readRDS("prod_model_outputs/march_forecast_lm.rds")
predictions_clim<-readRDS("prod_model_outputs/march_climate_lm.rds")
predictions_climate_and_ndvi<-readRDS("prod_model_outputs/march_climate_and_ndvi_lm.rds")


df_names<-paste0("pred", rep(seq(1987,2020,1)))

resids_sp_mat<-space_only_predictions[[3]]; names(resids_sp_mat)<-df_names
resids_lm_mat<-predictions_lm[[3]]; names(resids_lm_mat)<-df_names
resids_clim_mat<-predictions_clim[[3]]; names(resids_clim_mat)<-df_names
# resids_noaa_mat<-predictions_noaa3month_lm[[5]]; names(resids_noaa_mat)<-df_names
resids_clim_ndvi_mat<-predictions_climate_and_ndvi[[3]]; names(resids_clim_ndvi_mat)<-df_names



mape_fnc<-function(data) {mean(as.vector(abs(data)))}
mspe_fnc<-function(data) {mean(as.vector(data)^2)}
mbias_fnc<-function(data) {mean(as.vector(data))}

sp_mape<-apply(X= resids_sp_mat,2, FUN=mape_fnc)
sp_mspe<-apply(X= resids_sp_mat,2, FUN=mspe_fnc)
sp_mbias<-apply(X= resids_sp_mat,2, FUN=mbias_fnc)

real_mape<-apply(X= resids_lm_mat,2, FUN=mape_fnc)
real_mspe<-apply(X= resids_lm_mat,2, FUN=mspe_fnc)
real_mbias<-apply(X= resids_lm_mat,2, FUN=mbias_fnc)

clim_mape<-apply(X= resids_clim_mat,2, FUN=mape_fnc)
clim_mspe<-apply(X= resids_clim_mat,2, FUN=mspe_fnc)
clim_mbias<-apply(X= resids_clim_mat,2, FUN=mbias_fnc)

# noaa_mape<-apply(X= resids_noaa_mat,2, FUN=mape_fnc)
# noaa_mspe<-apply(X= resids_noaa_mat,2, FUN=mspe_fnc)
# noaa_mbias<-apply(X= resids_noaa_mat,2, FUN=mbias_fnc)

clim_and_ndvi_mape<-apply(X= resids_clim_ndvi_mat,2, FUN=mape_fnc)
clim_and_ndvi_mspe<-apply(X= resids_clim_ndvi_mat,2, FUN=mspe_fnc)
clim_and_ndvi_mbias<-apply(X= resids_clim_ndvi_mat,2, FUN=mbias_fnc)

mape<-cbind(sp_mape,real_mape,clim_mape,clim_and_ndvi_mape,seq(1987,2020,1))
colMeans(mape)

mspe<-cbind(sp_mspe,real_mspe,clim_mspe,clim_and_ndvi_mspe,seq(1987,2020,1))
colMeans(mspe)

mbias<-cbind(sp_mbias,real_mbias,clim_mbias,clim_and_ndvi_mbias,seq(1987,2020,1))
colMeans(mbias)

par(mfrow=c(1,1))

# Figure 4
png( "figures/time_series/mape.png")
plot(mape[,5],mape[,1], 'l',ylim=c(.35,1.75),  main= "Mean Absolute Prediction Error", col="black", cex.main=2.4,
     lwd=3,
     ylab= "Mean Absolute Predictive Error",
     xlab="Year", cex.lab=1.5, cex.axis=1.5) 
lines(mape[,5],mape[,2], 'l', col="blue",lwd=2)
lines(mape[,5],mape[,3], 'l', col="red", lwd=2)
lines(mape[,5],mape[,4], 'l', col="purple",lwd=2)
legend("topleft", pch=19,legend=c("null model", "forecast", "clim", "clim_and_ndvi"), 
       col=c("black","blue","red", "purple"), cex=2)
dev.off()


png( "figures/time_series/mspe.png")
plot(mspe[,5],mspe[,1], 'l',ylim=c(0,3),  main= "Mean Squared Predictive Errorby model", col="black", cex.main=3,
     lwd=2,
     ylab= "Mean Squared Predictive Error (standardized)",
     xlab="Year", cex.lab=1.5) 
lines(mspe[,5],mspe[,2], 'l',ylim=c(0,.5), col="blue")
lines(mspe[,5],mspe[,3], 'l',ylim=c(0,.5), col="red")
lines(mspe[,5],mspe[,4], 'l',ylim=c(0,.5), col="purple")
legend("topright", pch=19,legend=c("spatial only/null model", "forecast", "clim", "clim_and_ndvi"), col=c("black","blue","red", "purple"))
dev.off()


png( "figures/time_series/mean_bias.png")
plot(mbias[,5],mbias[,1], 'l',ylim=c(-1,1.5), main= "Mean Bias by model", col="black", cex.main=3,
     lwd=3,
     ylab= "Mean Bias (standardized)", cex.lab=2,cex.axis=1.5,
     xlab="Year", cex.lab=1.5) 
lines(mbias[,5],mbias[,2], 'l',ylim=c(-.5,.5), col="blue",lwd=2)
lines(mbias[,5],mbias[,3], 'l',ylim=c(-.5,.5), col="red",lwd=2)
lines(mbias[,5],mbias[,4], 'l',ylim=c(-.5,.5), col="purple",lwd=2)
legend("topright", pch=19,legend=c("spatial only/null model", "forecast", "clim", "clim_and_ndvi"), 
       col=c("black","blue","red", "purple"),cex=3)
dev.off()


mape_df<-as.data.frame(colMeans(mape))

mape_df$comp_to_null<-(mape_df[1,1]-mape_df[,1])/mape_df[1,1]*100
mape_df$comp_to_forecast<-(mape_df[2,1]-mape_df[,1])/mape_df[1,1]*100
mape_df[1:4,]

