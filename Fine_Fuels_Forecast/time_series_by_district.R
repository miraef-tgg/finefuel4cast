#This script makes hindcasts from 1987-2020

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#this seems potentially useful
latent_forecast_perc<-readRDS("Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds")

names(latent_forecast_perc)
Fspin_iters_10_sp<-latent_forecast_perc$CI10
Fspin_iters_90_sp<-latent_forecast_perc$CI90
Fspin_iters_est_sp<-latent_forecast_perc$mean
names(Fspin_iters_10_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat" , "district")
names(Fspin_iters_90_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat" , "district")
names(Fspin_iters_est_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat", "district")
dim(Fspin_iters_est_sp)
(districts<-as.vector(na.omit(unique(Fspin_iters_10_sp$district))))


nYears<-24


# View(Fspin_iters_90_sp[1:10,])

#save:
par(mfrow=c(2,2))
png("Fine_Fuels_Forecast/Figures/time_series_districts.png")

district_plots<-c("ARIZONA STRIP DISTRICT OFFICE", "Winnemucca District", "West Desert" , "Idaho Falls District Office")

par(mfrow=c(2,2))

for ( d in district_plots){

  hi<-as.data.frame(subset(Fspin_iters_90_sp, Fspin_iters_90_sp$district==d)[,1:24])
  lo<-as.data.frame(subset(Fspin_iters_10_sp, Fspin_iters_10_sp$district==d)[,1:24])
  est<-subset(Fspin_iters_est_sp, Fspin_iters_est_sp$district==d)[,1:24]

  lo<-apply(lo, 2, as.numeric ,na.rm=T)
  hi<-apply(hi, 2, as.numeric ,na.rm=T)
  est<-apply(est, 2, as.numeric ,na.rm=T)

  plot(x=seq(1998,2021), y=apply(lo, 2, mean ,na.rm=T), col="red", lty=2, type="l", ylim=c(-100,100),
       main = paste0(d),lwd=3, cex.lab=2,
       ylab="% above long-term normal",cex=1.5, cex.axis=2,
       xlab="Year")
  lines(x=seq(1998,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
  lines(x=seq(1998,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
  lines(x=seq(1998,2021), y=rep(0,24) , lty=2)
  legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2)

}


dev.off()

#to see all
# par(mfrow=c(1,1))
# for ( d in districts){
#   
#   hi<-as.data.frame(subset(Fspin_iters_90_sp, Fspin_iters_90_sp$district==d)[,1:24])
#   lo<-as.data.frame(subset(Fspin_iters_10_sp, Fspin_iters_10_sp$district==d)[,1:24])
#   est<-subset(Fspin_iters_est_sp, Fspin_iters_est_sp$district==d)[,1:24]
#   
#   lo<-apply(lo, 2, as.numeric ,na.rm=T)
#   hi<-apply(hi, 2, as.numeric ,na.rm=T)
#   est<-apply(est, 2, as.numeric ,na.rm=T)
#   
#   plot(x=seq(1998,2021), y=apply(lo, 2, mean ,na.rm=T), col="red", lty=2, type="l", ylim=c(-100,100),
#        main = paste0(d),lwd=3, cex.lab=2,
#        ylab="% above long-term normal",cex=1.5,
#        xlab="Year")
#   lines(x=seq(1998,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
#   lines(x=seq(1998,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
#   lines(x=seq(1998,2021), y=rep(0,24) , lty=2)
#   legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)
#   
# }

