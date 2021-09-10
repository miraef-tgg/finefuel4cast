
#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path
BL_folder<-paste0(dirname(dirname((dirname(current_path)))), "/Fuels_model/Bayesian_Land")
prod_folder<-paste0(dirname(dirname(dirname(((current_path))))),"/Prod_model/")

loc_keeps<-read.csv("Prod_Forecast_model/prod_model_outputs/loc_keeps.csv")

names(loc_keeps)<-c("X","long", "lat", "year", "keeps")
coords<-as.data.frame(loc_keeps)
coords<-subset(coords, coords$year==1987)
nrow(coords)==nrow(loc_keeps)/35
names(coords)
(nLocs<-nrow(coords))



##--------------------------- MAPS!!! -------------------------

blm_states<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/states.shp")
blm_admin<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/admin.shp")

coords_sp<-coords
coords_sp$lat<-as.numeric(coords_sp$lat);  coords_sp$long<-as.numeric(coords_sp$long)
coordinates(coords_sp)<-~long+lat

t<-intersect(coords_sp, blm_admin)
class(coords_sp)
class(blm_admin)

coords2<-as.data.frame(t)
# View(coords2[1:10,])
names(coords2)
keeps<-c("long", "lat","PARENT_N" )
coords3<-coords2[,keeps]
district_coords<-coords3


# write.csv(coords3,"G:/My Drive/finefuel4cast/Fuels_forecast/spin_up/coords.csv" )


