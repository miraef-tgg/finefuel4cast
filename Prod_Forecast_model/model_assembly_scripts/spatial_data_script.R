# This script formats spatial data downloaded from GEE and converts the tiff to a csv

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <- getActiveDocumentContext()$path
prod_folder<-paste0(dirname(dirname(current_path)))

image_sp<-stack("gee_4cast_data/prod_4cast_tiffs/spatial_gee_tiffs.tif")
image_df_orig<-as.data.frame(image_sp)
image_df<-image_df_orig

## ----------------------------------------------  coords ------------------------------------------------
image_df_coords<-as.data.frame(image_sp)
row_names<-seq(1,nrow(image_df_coords),1)
image_df_coords$row_names<-row_names
row_keeps<-subset(image_df_coords, !is.na(image_df_coords$elevation))
row_keeps_vec<-row_keeps$row_names 
temp_tiff <- (stack(raster("gee_4cast_data/prod_4cast_tiffs/spatial_gee_tiffs.tif")))
temp_tiff[is.na(temp_tiff[])] <- 999 
coords<-rasterToPoints(temp_tiff)[,1:2]
coords<-as.data.frame(coords)
coords$row_names<-seq(1,nrow(coords),1)
coords_final<-subset(coords, coords$row_names %in% row_keeps_vec)
coords_final<-coords_final[,1:2]
names(coords_final)<-c("long", "lat")
dim(coords_final)

## ----------------------------------------------  subset data ---------------------------------------------- 

image_df$row_names<-seq(1,nrow(image_df_orig),1)
image_df<-subset(image_df, image_df$row_names %in% row_keeps_vec)
image_df<-image_df[,1:(ncol(image_df)-1)]
dim(image_df)

## ---------------------------------------------- soil classes ------------------------------------------------

# 1	d5c36b	Cl
# 2	b96947	SiCl
# 3	9d3706	SaCl
# 4	ae868f	ClLo
# 5	f86714	SiClLo
# 6	46d143	SaClLo
# 7	368f20	Lo
# 8	3e5a14	SiLo
# 9	ffd557	SaLo
# 10	fff72e	Si
# 11	ff5a9d	LoSa
# 12	ff005b	Sa

soil_df<-image_df[,1:14]
soil_df$text<-apply(X=soil_df[,3:8], MARGIN=1,FUN=median)
soil_df$text_bin<-ifelse(soil_df$text==7, soil_df$text_bin<-1, soil_df$text_bin<-0)
soil_df$bulk_dens<-apply(X=soil_df[,9:14], MARGIN=1,FUN=mean)

soil_df$z_ssm<-(soil_df$ssm-mean(soil_df$ssm,na.rm=TRUE))/sd(soil_df$ssm,na.rm=TRUE)
soil_df$z_bulk_dens<-(soil_df$bulk_dens-mean(soil_df$bulk_dens,na.rm=TRUE))/sd(soil_df$bulk_dens,na.rm=TRUE)

table(soil_df$text)
table(soil_df$text_bin)
# hist(soil_df$z_bulk_dens)
# hist(soil_df$z_ssm)
# hist(soil_df$smp)


keeps<-c("z_ssm", "smp", "z_bulk_dens", "text_bin")
soil_df_final<-soil_df[,keeps]

## ----------------------------------------------  elev  ------------------------------------------------
elev_df<-as.data.frame(image_df[,15])
names(elev_df)<-"elev"
elev_df$z_elev <- (elev_df$elev - mean(elev_df$elev))/sd(elev_df$elev)


## ---------------------------------------------- final ------------------------------------------------

final_df<-cbind(soil_df_final, elev_df$z_elev, coords_final)
names(final_df)<-c("z_ssm", "smp", "z_bulk_dens", "text_bin", "z_elev", "long", "lat")
col_order <- c("long", "lat","z_elev","z_ssm", "smp", "z_bulk_dens", "text_bin")
final_df <- final_df[, col_order]

dim(final_df)
head(final_df)

write.csv(final_df, "gee_4cast_data/prod_4cast_csvs/gee_spatial_data.csv", row.names=F)

