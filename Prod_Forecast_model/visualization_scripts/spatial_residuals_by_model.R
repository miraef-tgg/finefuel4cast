## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

space_only_predictions<-readRDS( "prod_model_outputs/march_spatial_only_lm.rds")
predictions_lm<-readRDS("prod_model_outputs/march_forecast_lm.rds")
predictions_clim<-readRDS("prod_model_outputs/march_climate_lm.rds")
predictions_climate_and_ndvi<-readRDS("prod_model_outputs/march_climate_and_ndvi_lm.rds")

models<-list(space_only_predictions,predictions_lm,predictions_clim,predictions_climate_and_ndvi)
save_locs<- list("/spatial_only_model/","/forecast/","/climate/","/climate_and_ndvi/")


blm_states<-readOGR( "gee_4cast_data/gis_data/states.shp")
blm_admin<-readOGR("gee_4cast_data/gis_data/admin.shp")

source(file = ( "visualization_scripts/model_errors_time_series.R"))

nYears<-34
# nYears<-1
# i<-1
## ---------------------------------------- Variograms ---------------------------------

# for (i in 1:nYears){
#   print(1986+i)
#   cur_df<-as.data.frame(cbind(space_only_predictions[[3]][,i], predictions_lm[[4]]))
#   names(cur_df)<-c("resid", "long", "lat")
#   name<-paste0("Variogram: test year = ", 1986+i)
#   name2<-paste0("Vgram", 1986+i)
#   Vout<-variogram(cur_df$resid~1,loc=~lat+long, data=cur_df,width=.01)
#   # png(paste0(vis_fold, "/spatial_only_model/",name2, ".png"))
#   print(plot(Vout, main=name))
#   # dev.off()
# }

## ---------------------------------------- per pixel residuals by year ---------------------------------


#standard colors 
cols1<-colorRampPalette(c("green", "yellow"))
cols2<-colorRampPalette(c("yellow", "red"))

#low/hi resid vals

for (m in 1:length(models)){
  predictions_model<-models[[m]]
  save_loc<-save_locs[[m]]
  
  min_val<-min(predictions_model[[3]])
  max_val<-max(predictions_model[[3]])
  hist(predictions_model[[3]])
  hist(predictions_model[[1]])
  
  l1<-seq(round(min_val,2),0,.01)
  l2<-seq(0,round(max_val,2),.01)
  
  col_df1<-data.frame(c_val=l1,color.name=cols1(length(l1)))
  col_df2<-data.frame(c_val=l2,color.name=cols2(length(l2)))
  col_df<-rbind(col_df1,col_df2)
  
  resid_list<-list()
  
  # spatial only loop
  for (i in 1:nYears){
    cur_df<-as.data.frame(cbind(predictions_model[[3]][,i], predictions_lm[[4]]))
    names(cur_df)<-c("resid", "long", "lat")
    
    print(i+1986)
    cur_df$round<-as.character(round((cur_df$resid),2))
    col_df$c_val<-as.character(col_df$c_val)
    col_resid<-merge(cur_df, col_df, by.x="round",by.y="c_val")
    sp<-col_resid; sp$lat<-as.numeric(sp$lat);  sp$long<-as.numeric(sp$long)
    coordinates(sp)<-~long+lat;  sp@proj4string<-CRS("+init=epsg:4326")
    name<-paste0("resids",i+1986)
    png(paste0("figures/model_residuals/", save_loc,name, ".png"))
    layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))
    
    print(plot('n',x=sp$long,y=sp$lat,xlab="Longitude", ylab="Latitude", 
               main=paste("Residuals on ", i+1986," predictions \n  MAPE: ", 
                          round(mape[i,m],2), "  MSPE: ", round(mspe[i,m],2), " MBIAS: ", round(mbias[i,m],2))))
    print(abline(h = 40, col = "gray", lwd = 800))
    print(points(x=sp$long,y=sp$lat,col=sp$color.name,pch=15,cex=1.5))
    
    print(lines(blm_states,col="black"))
    par(mar=c(5.1,4.1,4.1,2.1))
    xl <- 1;yb <- 1;xr <- 1.5;yt <- 2
    
    par(mar=c(5.1,0.5,4.1,0.5))
    plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
    rect(
      xl,
      head(seq(yb,yt,(yt-yb)/10),-1),
      xr,
      tail(seq(yb,yt,(yt-yb)/10),-1),
      col=c(cols1(5), cols2(5))
    )
    q10<-round(quantile(as.numeric(col_df$c_val),seq(0,.9,.1)),2)
    
    mtext(q10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
    dev.off()
    
    
  }
}
 
# # ## --------------------------------------------- TS resid patterns --------------------------------------------- 
# # 
# # # in resids (are some places always under/over estimated?)
# df_names<-paste0(names(predictions_lasso[[1]]), rep(seq(1986,2019,1),each=7))
# df_names
# hist(predictions_lasso[[1]]$resid, breaks=50)
# 
# rand<-sample(x=1:nrow(predictions_lasso[[3]]), size=8000)
# plot(predictions_lasso[[1]]$pred[rand], predictions_lasso[[1]]$agb[rand],pch=19, col=rgb(0,0,1,.6),cex=.1) #regression diffusion?
# abline(a=0,b=1, col="red")
# 
# new2<-subset(new, select=c(paste0("resid", "_", seq(1,33,1))))
# 
# resids_lasso<-do.call(args=predictions_lasso, what=cbind); names(resids_lasso)<-df_names
# resids_lasso_mat<-resids_lasso[, names(resids_lasso) %in% c(names(resids_lasso)[(grepl(as.character("resid"), names(resids_lasso)))])]
# 
# rand_10<-sample(1:nrow(resids_lasso_mat),10)
# ts_10<-resids_lasso_mat[rand_10,]
# 
# png(paste0(vis_fold, "/lasso/resid_ts_1.png"))
# plot.ts(t(ts_10), main= "10 randomly drawn time series plots", xy.labels="h")
# dev.off()
# 
# 
# n<-1000
# rand_10<-sample(1:nrow(resids_lasso_mat),n)
# ts_10<-resids_lasso_mat[rand_10,]
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# png(paste0(vis_fold, "/lasso/resid_ts_2.png"))
# plot(x=seq(1987,2019,1), y=(ts_10[1,]), type="l", col="black", ylim=c(-1,1), ylab="residual for indiv loc", xlab="year", main="Residual time series for 1000 random locations")
# for (i in 1:n){
#   lines(x=seq(1987,2019,1), y=(ts_10[i,]), type="l", col=col_vector[i])
# }
# 
# dev.off()
# 
# 
# # ## ---------------------------------------------Autocorr per pixel  --------------------------------------------- 
# 
# rand_1000<-sample(1:nrow(resids_lasso_mat),1000)
# acf_df<-data.frame(matrix(NA, nrow=1000,ncol=11))
# ts_big<-t(resids_lasso_mat)
# for( i in 1:length(rand_1000)){
#   r<-(rand_1000)[i]
#   a<-(acf(ts_big[,r],lag.max = 10,plot=FALSE)$acf)
#   acf_df[i,]<-t(as.vector(acf((ts_big[,r]),lag.max = 10,plot=FALSE)$acf))
# }
# 
# colMeans(acf_df)
# 
