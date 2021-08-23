
#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


#fitted model
forecast_rds<-readRDS("Prod_Forecast_model/prod_model_outputs/march_forecast_lm.rds")
forecast_mod<-forecast_rds$mod_fit


#productivity forecast
prod_point_estimate<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/prod_cast_1988_2021.csv") 


#latent fine fuel forecast (standardized and % above normal)
fuelcast_stan<-read.csv("Fine_Fuels_Forecast/output_data/latent_forecast_distrib.csv") 
fuelcast_perc<-readRDS("Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds") 
names(fuelcast_perc)<-c("CI10", "CI90", "mean")

blm_states<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/states.shp")
blm_admin<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/admin.shp")


model_df2<-read.csv("Prod_Forecast_model/gee_4cast_data/model_csvs/march_all_model_csv.csv")


spin_mean<-fuelcast_perc$mean[,24]
spin_90<-fuelcast_perc$CI90[,24]
spin_10<-fuelcast_perc$CI10[,24]

coords<-fuelcast_perc$mean[,25:26]
names(coords)<-c("lat", "long")
# spin_mean<-as.numeric()
# spin_90<-apply(spin_90, 1,as.numeric, na.rm=T)
# spin_10<-apply(spin_10, 1,as.numeric, na.rm=T)


spin_mean<-round(as.numeric(spin_mean),2)
spin_90<-round(as.numeric(spin_90),2)
spin_10<-round(as.numeric(spin_10),2)


##--------------------------- MAPS!!! -------------------------


# make standardized set of colors for this year's maps
cols1<-colorRampPalette(c("green", "yellow", "red"))

min_val<-round(min(spin_10, na.rm=T),2)
max_val<-round(max(spin_90,na.rm=T),2)
l1<-seq(min_val,max_val,.01)
col_df<-data.frame(c_val=l1,color.name=cols1(length(l1)))

#### make legend 
leg_vals<-round(seq(min_val,max_val,length.out=5),2)
leg1<-matrix(nrow=5,ncol=2)
leg1[1,]<-c(leg_vals[1],subset(col_df, round(as.numeric(col_df$c_val),2)==(leg_vals[1]))$color.name[1])
leg1[2,]<-c((leg_vals[2]), subset(col_df, round(as.numeric(col_df$c_val),2)==(leg_vals[2]))$color.name[1])
leg1[3,]<-c((leg_vals[3]), subset(col_df, round(as.numeric(col_df$c_val),2)==(leg_vals[3]))$color.name[1])
leg1[4,]<-c((leg_vals[4]), subset(col_df, round(as.numeric(col_df$c_val),2)==(leg_vals[4]))$color.name[1])
leg1[5,]<-c((leg_vals[5]), subset(col_df, round(as.numeric(col_df$c_val),2)==(leg_vals[5]))$color.name[1])
leg1


par(mfrow=c(1,3))
names(col_df)<-c("fuel", "color.name")
#new sp object and map for 10% data
spin_sp<-as.data.frame(cbind(spin_10, coords)); names(spin_sp)<-c("fuel", "long", "lat")
col_resid<-plyr::join(spin_sp, col_df, by="fuel")
sp3<-col_resid; sp3$lat<-as.numeric(sp3$lat);  sp3$long<-as.numeric(sp3$long)
coordinates(sp3)<-~long+lat
print(plot('n',x=sp3$long,y=sp3$lat,xlab="Longitude", ylab="Latitude", 
           main=paste("10% CI fine fuel forecast"), cex.lab=2))
print(abline(h = 42, col = "gray", lwd = 700))
print(points(x=sp3$long,y=sp3$lat,col=sp3$color.name,pch=15,cex=1.5))
# lines(blm_admin, col="darkgray", lwd=2)
# lines(blm_states,col="darkgray", lwd=3)



#join colors and values into df, make into spatial object
spin_sp<-as.data.frame(cbind(spin_mean, coords)); names(spin_sp)<-c("fuel", "long", "lat")
col_df$fuel<-as.character(col_df$fuel)
col_resid<-plyr::join(spin_sp, col_df, by="fuel")
sp<-col_resid; sp$lat<-as.numeric(sp$lat);  sp$long<-as.numeric(sp$long)
coordinates(sp)<-~long+lat


#map of mean estimates
print(plot('n',x=sp$long,y=sp$lat,xlab="", ylab="", 
           main=paste("Fine Fuel Forecast: \n  march 2021 "), cex.main=2))
print(abline(h = 42, col = "gray", lwd = 700))
print(points(x=sp$long,y=sp$lat,col=sp$color.name,pch=15,cex=1.5))
# lines(blm_admin, col="darkgray", lwd=2)
# lines(blm_states,col="darkgray", lwd=3)


#new sp object and map for 90% data
spin_sp<-as.data.frame(cbind(spin_90, coords)); names(spin_sp)<-c("fuel", "long", "lat")
col_resid<-plyr::join(spin_sp, col_df, by="fuel")
sp2<-col_resid; sp2$lat<-as.numeric(sp2$lat);  sp2$long<-as.numeric(sp2$long)
coordinates(sp2)<-~long+lat
print(plot('n',x=sp2$long,y=sp2$lat,xlab="", ylab="", 
           main=paste("90% CI fine fuel forecast ")))
print(abline(h = 42, col = "gray", lwd = 700))
print(points(x=sp2$long,y=sp2$lat,col=sp2$color.name,pch=15,cex=.5))
print(legend("topright",legend=paste0(c(leg1[,1]), "%"), col=leg1[,2], pch=19, bg="lightgray", cex=1.5, 
             title = " % above long term average"))


# lines(blm_admin, col="darkgray", lwd=2)
# lines(blm_states,cCol="darkgray", lwd=3)

# dev.off()


