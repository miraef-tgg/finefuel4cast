#This script makes hindcasts for prod data from 1987-2020 by district

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

coords<-readRDS(paste0("prod_model_outputs/march_spatial_only_lm.rds"))$coord_df
district_coords<-read.csv("G:/My Drive/fine_fuel_forecast/finefuel4cast/Fine_Fuels_Forecast/output_data/district_coords.csv" )
# coords$join1<-paste0(coords$long,"_", coords$lat)
# district_coords$join1<-paste0(district_coords$long,"_", district_coords$lat)
coords$long<-substring(as.character(coords$long),0,8)
coords$lat<-substring(as.character(coords$lat),0,8)
district_coords$long<-substring(as.character(district_coords$long),0,8)
district_coords$lat<-substring(as.character(district_coords$lat),0,8)

coords2<-plyr::join(coords, district_coords,  type="left", match="first")
dim(coords2)

names(coords2)<-c("lat", "long", "X", "district")

prod_forecast<-read.csv("prod_model_outputs/forecast2021/prod_cast_1988_2021_uncert.csv")
dim(prod_forecast)

#--------------backtransform prod_forecast--------------------
agb<-readRDS(paste0("prod_model_outputs/march_all_model_csv.csv"))
model_df2<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")

agb<-model_df2$agb
agb_m<-matrix(NA,nrow=nLocs,ncol=34)

for (i in 1:34){
  agb_m[1:nLocs,i]<-agb[(nLocs*(i-1)+1):(i*nLocs)]
  # print(paste0((nLocs*(i-1)+1), " : " , (i*nLocs)))
}

agb_m[1:10,]
long_term_sd<-apply(agb_m,1,sd, na.rm=T)
long_term_mean<-apply(agb_m,1,mean)
long_term_sd<-rep(long_term_sd, 34)
long_term_mean<-rep(long_term_mean, 34)

Fspin_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_90_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_10_raw<-matrix(NA,nrow=nLocs,ncol=24)
prod_forecast_raw<-matrix(NA, nrow=nrow(prod_forecast), ncol=ncol(prod_forecast))
prod_forecast_trans<-matrix(NA, nrow=nrow(prod_forecast), ncol=ncol(prod_forecast))
prod_perc<-matrix(NA, nrow=nrow(prod_forecast), ncol=ncol(prod_forecast))

dim(prod_forecast);dim(prod_forecast_raw)
for ( i in 1:ncol(prod_forecast_raw)){
  prod_forecast_raw[,i]<-(prod_forecast[,i] * long_term_sd)+long_term_mean
}

for ( i in 1:ncol(prod_forecast_raw)){
  prod_perc[,i]<-(prod_forecast_raw[,i]-long_term_mean)/long_term_sd*100
}


head(prod_forecast)
head(prod_perc)
prod_forecast<-prod_perc

######--------------make different conf intervals -------------


Fspin_iters_10_sp<-apply(prod_forecast, 1, quantile, .05, na.rm=T)
(nLocs<-length(Fspin_iters_10_sp)/34)
Fspin_iters_90_sp<-apply(prod_forecast, 1, quantile, .95, na.rm=T)
Fspin_iters_est_sp<-apply(prod_forecast, 1,mean)

Fspin_iters_10_sp<-as.data.frame(Fspin_iters_10_sp)
Fspin_iters_10_sp$lat<-rep(coords2$lat, 34)
Fspin_iters_10_sp$long<-rep(coords2$long, 34)
Fspin_iters_10_sp$district<-rep(coords2$district, 34)

Fspin_iters_90_sp<-as.data.frame(Fspin_iters_90_sp)
Fspin_iters_90_sp$lat<-rep(coords2$lat, 34)
Fspin_iters_90_sp$long<-rep(coords2$long, 34)
Fspin_iters_90_sp$district<-rep(coords2$district, 34)

Fspin_iters_est_sp<-as.data.frame(Fspin_iters_est_sp)
Fspin_iters_est_sp$lat<-rep(coords2$lat, 34)
Fspin_iters_est_sp$long<-rep(coords2$long, 34)
Fspin_iters_est_sp$district<-rep(coords2$district, 34)

(districts<-as.vector(na.omit(unique(Fspin_iters_10_sp$district))))

#save:
par(mfrow=c(2,2))
# png("Fine_Fuels_Forecast/Figures/time_series_districts.png")

district_plots<-c("ARIZONA STRIP DISTRICT OFFICE", "Winnemucca District", "West Desert" , "Idaho Falls District Office")
d<-"Winnemucca District"
par(mfrow=c(1,1))

for ( d in district_plots){
  p90<-na.omit(Fspin_iters_90_sp[(Fspin_iters_90_sp["district"]==d),], cols=lat)
  p90$grp<-paste0(p90$lat, p90$long)

p90_clean <- p90 %>%
  group_by(p90$grp) %>%
  tally()
names(p90_clean)<-c("grp", "counts")
p90_cleaner<-plyr::join(p90, p90_clean)
p90<-subset(p90_cleaner, p90_cleaner$counts==34)

 p10<-na.omit(Fspin_iters_10_sp[Fspin_iters_10_sp["district"]==d,])
 p10$grp<-paste0(p10$lat, p10$long)
 
 p10_clean <- p10 %>%
   group_by(p10$grp) %>%
   tally()
 names(p10_clean)<-c("grp", "counts")
 p10_cleaner<-plyr::join(p10, p10_clean)
 p10<-subset(p10_cleaner, p10_cleaner$counts==34) 
 
 pmean<-na.omit(Fspin_iters_est_sp[Fspin_iters_est_sp["district"]==d,])
 pmean$grp<-paste0(pmean$lat, pmean$long)
 
 pmean_clean <- pmean %>%
   group_by(pmean$grp) %>%
   tally()
 names(pmean_clean)<-c("grp", "counts")
 pmean_cleaner<-plyr::join(pmean, pmean_clean)
 pmean<-subset(pmean_cleaner, pmean_cleaner$counts==34)
 table((Fspin_iters_90_sp["district"]==d))
  
  (temp_locs<-nrow(p90)/34)
  lo<-matrix(NA,nrow=nrow(p90),ncol=34)
  hi<-matrix(NA,nrow=nrow(p90),ncol=34)
  est<-matrix(NA,nrow=nrow(p90),ncol=34)
  
  p90[1:10,]
  Fspin_iters_90_sp[1:10,]

  for ( i in 1:34){
    lo[,i]<-as.vector(p10$Fspin_iters_10_sp[((i-1)*temp_locs+1):(i*temp_locs)])
    hi[,i]<-as.vector(p90$Fspin_iters_90_sp[((i-1)*temp_locs+1):(i*temp_locs)])
    est[,i]<-as.vector(pmean$Fspin_iters_est_sp[((i-1)*temp_locs+1):(i*temp_locs)])
    # print(((i-1)*temp_locs+1))
    # print(i*temp_locs)
  }
  head(lo)
  
  # lo<-apply(lo, 2, as.numeric ,na.rm=T)
  # hi<-apply(hi, 2, as.numeric ,na.rm=T)
  # est<-apply(est, 2, as.numeric ,na.rm=T)

  plot(x=seq(1988,2021), y=apply(lo,2,mean,na.rm=T), col="red", lty=2, type="l",
       main = paste0(d),lwd=3, cex.lab=2,
       ylab="% above long-term normal",cex=1.5, cex.axis=2,
       xlab="Year", ylim=c(min(apply(lo,2,mean,na.rm=T)), max(apply(hi,2,mean,na.rm=T))))
  lines(x=seq(1988,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
  lines(x=seq(1988,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
  legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2)

}


# dev.off()

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

