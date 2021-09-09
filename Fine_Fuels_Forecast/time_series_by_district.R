#This script makes hindcasts from 1987-2020

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#this seems potentially useful
latent_forecast_perc<-readRDS("Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds")

dim(Fspin_iters_10)
names(Fspin_iters_10_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat" , "district")
names(Fspin_iters_90_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat" , "district")
names(Fspin_iters_est_sp)<-c(paste0("fuel", seq(1998,2021)), "long", "lat", "district")
dim(Fspin_iters_est_sp)
(districts<-as.vector(na.omit(unique(latent_forecast_perc$PARENT_N))))
names(Fspin_iters_90_sp)

nYears<-24


# View(Fspin_iters_90_sp[1:10,])

#save:
png("Prod_Forecast_model/Figures/time_series_districts.png")


for ( d in districts){

  hi<-as.data.frame(subset(Fspin_iters_90_sp, Fspin_iters_90_sp$district==d)[,1:24])
  lo<-as.data.frame(subset(Fspin_iters_10_sp, Fspin_iters_10_sp$district==d)[,1:24])
  est<-subset(Fspin_iters_est_sp, Fspin_iters_est_sp$district==d)[,1:24]

  lo<-apply(lo, 2, as.numeric ,na.rm=T)
  hi<-apply(hi, 2, as.numeric ,na.rm=T)
  est<-apply(est, 2, as.numeric ,na.rm=T)

  plot(x=seq(1998,2021), y=apply(lo, 2, mean ,na.rm=T), col="red", lty=2, type="l", ylim=c(-100,100),
       main = paste0(d),lwd=3, cex.lab=2,
       ylab="% above long-term normal",cex=1.5,
       xlab="Year")
  lines(x=seq(1998,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
  lines(x=seq(1998,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
  lines(x=seq(1998,2021), y=rep(0,24) , lty=2)
  legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)

}


dev.off()

#show them

for ( d in districts){
  
  hi<-as.data.frame(subset(Fspin_iters_90_sp, Fspin_iters_90_sp$district==d)[,1:24])
  lo<-as.data.frame(subset(Fspin_iters_10_sp, Fspin_iters_10_sp$district==d)[,1:24])
  est<-subset(Fspin_iters_est_sp, Fspin_iters_est_sp$district==d)[,1:24]
  
  lo<-apply(lo, 2, as.numeric ,na.rm=T)
  hi<-apply(hi, 2, as.numeric ,na.rm=T)
  est<-apply(est, 2, as.numeric ,na.rm=T)
  
  plot(x=seq(1998,2021), y=apply(lo, 2, mean ,na.rm=T), col="red", lty=2, type="l", ylim=c(-100,100),
       main = paste0(d),lwd=3, cex.lab=2,
       ylab="% above long-term normal",cex=1.5,
       xlab="Year")
  lines(x=seq(1998,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
  lines(x=seq(1998,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
  lines(x=seq(1998,2021), y=rep(0,24) , lty=2)
  legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)
  
}

