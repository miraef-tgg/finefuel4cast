# mean long term residuals
# mean correlation

#loading data and pcks

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, gstat,stringr,raster,rstudioapi, rgdal, spacetime,ape,FRK,glmnet,install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# month<-"march"

pred1<-readRDS(paste0("prod_model_outputs/march_forecast_lm.rds"))

model_df_orig<-read.csv(("gee_4cast_data/model_csvs/march_all_model_csv.csv"))

blm_states<-readOGR("gee_4cast_data/gis_data/states.shp")
blm_admin<-readOGR( "gee_4cast_data/gis_data/admin.shp")
nYears<-34

## -----------------------------------------1) Long Term Prod  ------------------------------------
raw_dat<-model_df_orig[, c("agb", "yr", "long", "lat")]
nLocs<-nrow(raw_dat)/34
raw_prod<-matrix(NA, nrow=nLocs, ncol=34)


i<-1;r<-1;j<-1
for ( r in 1:nrow(raw_dat)){
  raw_prod[i,j]<-raw_dat$agb[r]
  i<-i+1
  if (i>nLocs){
    j=j+1
    i=1
  }
}  

df_names<-paste0(names(raw_prod[[1]]), rep(seq(1987,2019,1),each=6))
cols1<-colorRampPalette(c("red", "yellow", "green"))


long_term_prod<-rowMeans(raw_prod, na.rm=T)
max_val<-round(max(long_term_prod, na.rm=T),0)
min_val<-round(min(long_term_prod, na.rm=T),0)
l<-seq(round(min_val,0),round(max_val,0),1)

col_df<-data.frame(c_val=l,color.name=cols1(length(l)))
col_df$c_val<-round(col_df$c_val,0)
avg_agb_round<-as.data.frame(round(long_term_prod,0));names(avg_agb_round)<-"avg_agb_round"
avg_agb_round$long<-pred1$coord_df$long
avg_agb_round$lat<-pred1$coord_df$lat

long_term_prod2<-merge(avg_agb_round,col_df, by.x=as.character("avg_agb_round"), by.y=as.character("c_val"), all.x=TRUE)

avg_agb_round$avg_agb_round<-as.character(avg_agb_round$avg_agb_round)
col_df$c_val<-as.character(col_df$c_val)

order_df<-long_term_prod2[order(long_term_prod2$avg_agb_round),]
leg1<-as.data.frame(matrix(NA, nrow=3,ncol=2))
leg1[1,]<-c(order_df$avg_agb_round[1], order_df$color.name[1])
leg1[2,]<-c(order_df$avg_agb_round[round(nrow(order_df)*2/5,0)], order_df$color.name[round(nrow(order_df)*2/5,0)])
leg1[3,]<-c(order_df$avg_agb_round[round(nrow(order_df)*3/5,0)], order_df$color.name[round(nrow(order_df)*3/5,0)])
leg1[4,]<-c(order_df$avg_agb_round[round(nrow(order_df)*4/5,0)], order_df$color.name[round(nrow(order_df)*4/5,0)])
leg1[5,]<-c(order_df$avg_agb_round[(nrow(order_df))], order_df$color.name[(nrow(order_df))])
leg1

sp<-long_term_prod2
sp$lat<-as.numeric(sp$lat)
sp$long<-as.numeric(sp$long)
coordinates(sp)<-~long+lat
sp@proj4string<-CRS("+init=epsg:4326")


png( "figures/summary_figures/mean_productivity.png")
layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))

plot('n',x=sp$long,y=sp$lat,xlab="", 
     ylab="", main="Mean productivity 1986-2020", xlim=c(-123,-110),ylim=c(33,50),
      yaxt="none",xaxt="none")
print(abline(h = 30, col = "lightblue", lwd = 800))
print(abline(h = 50, col = "wheat", lwd = 200))
points(x=sp$long,y=sp$lat, pch=15, cex=.7,col=sp$color.name)

plot(blm_admin, col="lightgray",xlim=c(-123,-110),ylim=c(33,50), add=TRUE)
points(x=sp$long,y=sp$lat,
     pch=15, cex=.8,col=sp$color.name, xlim=c(-123,-110),ylim=c(33,50))
lines(blm_admin, col="gray")
lines(blm_states,col="darkgray")

par(mar=c(5.1,4.1,4.1,2.1))
xl <- 1;yb <- 1;xr <- 1.5;yt <- 2

par(mar=c(5.1,0.5,4.1,0.5))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/10),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/10),-1),
  col=cols1(10)
)
q10<-round(quantile(as.numeric(col_df$c_val),seq(0,.9,.1)),1)

mtext(q10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
dev.off()

## -----------------------------------------2)  Long Term prod SD  (raw) ------------------------------------

cols1<-colorRampPalette(c("yellow", "red"))


long_term_sd_prod<-apply(raw_prod, 1,sd, na.rm=T)
max_val<-round(max(long_term_sd_prod, na.rm=T),0)
min_val<-round(min(long_term_sd_prod, na.rm=T),0)
l<-seq(round(min_val,0),round(max_val,0),1)

col_df<-data.frame(c_val=l,color.name=cols1(length(l)))
col_df$c_val<-round(col_df$c_val,0)
sd_agb_round<-as.data.frame(round(long_term_sd_prod,0));names(sd_agb_round)<-"sd_agb_round"
sd_agb_round$long<-pred1$coord_df$long
sd_agb_round$lat<-pred1$coord_df$lat

long_term_sd_prod2<-merge(sd_agb_round,col_df, by.x=as.character("sd_agb_round"), by.y=as.character("c_val"), all.x=TRUE)

sd_agb_round$sd_agb_round<-as.character(sd_agb_round$sd_agb_round)
col_df$c_val<-as.character(col_df$c_val)

order_df<-long_term_sd_prod2[order(long_term_sd_prod2$sd_agb_round),]
leg1<-as.data.frame(matrix(NA, nrow=3,ncol=2))
leg1[1,]<-c(order_df$sd_agb_round[1], order_df$color.name[1])
leg1[2,]<-c(order_df$sd_agb_round[round(nrow(order_df)*2/5,0)], order_df$color.name[round(nrow(order_df)*2/5,0)])
leg1[3,]<-c(order_df$sd_agb_round[round(nrow(order_df)*3/5,0)], order_df$color.name[round(nrow(order_df)*3/5,0)])
leg1[4,]<-c(order_df$sd_agb_round[round(nrow(order_df)*4/5,0)], order_df$color.name[round(nrow(order_df)*4/5,0)])
leg1[5,]<-c(order_df$sd_agb_round[(nrow(order_df))], order_df$color.name[(nrow(order_df))])
leg1

sp<-long_term_sd_prod2
sp$lat<-as.numeric(sp$lat)
sp$long<-as.numeric(sp$long)
coordinates(sp)<-~long+lat
sp@proj4string<-CRS("+init=epsg:4326")


png("figures/summary_figures/mean_standard_dev.png")
layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))

plot('n',x=sp$long,y=sp$lat,xlab="", 
     ylab="", main="Mean SD 1986-2020", xlim=c(-123,-110),ylim=c(33,50),
     yaxt="none",xaxt="none")
print(abline(h = 30, col = "lightblue", lwd = 800))
print(abline(h = 50, col = "wheat", lwd = 200))
points(x=sp$long,y=sp$lat, pch=15, cex=.7,col=sp$color.name)

plot(blm_admin, col="lightgray",xlim=c(-123,-110),ylim=c(33,50), add=TRUE)
points(x=sp$long,y=sp$lat,
       pch=15, cex=.8,col=sp$color.name, xlim=c(-123,-110),ylim=c(33,50))
lines(blm_admin, col="gray")
lines(blm_states,col="darkgray")

par(mar=c(5.1,4.1,4.1,2.1))
xl <- 1;yb <- 1;xr <- 1.5;yt <- 2

par(mar=c(5.1,0.5,4.1,0.5))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/10),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/10),-1),
  col=cols1(10)
)
q10<-round(quantile(as.numeric(col_df$c_val),seq(0,.9,.1)),1)

mtext(q10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
dev.off()


## ----------------------------------------- 3) Mean Correlation; from forecast------------------------------------
preds_forecast<-pred1$prod_mean
prod<-pred1$agb_m
forecast_corr<-vector()
for (i in 1:nrow(prod)){
  forecast_corr[i]<-cor(preds_forecast[i,1:34],prod[i,1:34])
}

length(forecast_corr)

#check
# forecast_corr[3243]
# a<-((prod[3243,1:34]))
# b<-((preds_forecast[3243,1:34]))
# cor(a,b)

min_val<-round(min(forecast_corr),2)
max_val<-round(max(forecast_corr),2)
l1<-seq(round(min_val,2),0,.001)
l2<-seq(0,round(max_val,2),.001)

cols1<-colorRampPalette(c("green", "yellow"))
cols2<-colorRampPalette(c("yellow", "red"))
cols_new<-colorRampPalette(c("green", "yellow", "yellow2","red"))

col_df1<-data.frame(c_val=l1,color.name=cols1(length(l1)))
col_df2<-data.frame(c_val=l2,color.name=cols2(length(l2)))
col_df<-rbind(col_df1,col_df2)

#### standard legend

leg1<-matrix(nrow=5,ncol=2)
leg1[1,]<-c((min_val), subset(col_df, round(as.numeric(col_df$c_val),3)==(min_val))$color.name[1])
leg1[2,]<-c((-.10), subset(col_df, round(as.numeric(col_df$c_val),3)==(-.1))$color.name[1])
leg1[3,]<-c((0), subset(col_df, round(as.numeric(col_df$c_val),3)==(0))$color.name[1])
leg1[4,]<-c((.50), subset(col_df, round(as.numeric(col_df$c_val),3)==(.5))$color.name[1])
leg1[5,]<-c((.90), subset(col_df, round(as.numeric(col_df$c_val),3)==(.9))$color.name[1])

forecast_corr<-as.data.frame(as.character(round((forecast_corr),3))); names(forecast_corr)<-"round"
forecast_corr$long<-pred1$coord_df$long
forecast_corr$lat<-pred1$coord_df$lat

col_df$c_val<-as.character(round(as.numeric(col_df$c_val),3))
col_resid<-merge(forecast_corr, col_df, by.x="round",by.y="c_val",all.x=TRUE )
dim(col_resid)
sp<-col_resid;
coordinates(sp)<-~long+lat;  sp@proj4string<-CRS("+init=epsg:4326")



png( "figures/summary_figures/correlation_forecast_model.png")
layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))

print(plot('n',x=sp$long,y=sp$lat,xlab="", 
     ylab="", main=paste0("Correlation between forecast model and productivity \n 1986-2020"), xlim=c(-123,-110),ylim=c(33,50),
     yaxt="none",xaxt="none"))
print(abline(h = 30, col = "lightblue", lwd = 800))
print(abline(h = 50, col = "wheat", lwd = 200))
print(points(x=sp$long,y=sp$lat, pch=15, cex=.7,col=sp$color.name))

plot(blm_admin, col="lightgray",xlim=c(-123,-110),ylim=c(33,50), add=TRUE)
points(x=sp$long,y=sp$lat,
       pch=15, cex=.7,col=sp$color.name, xlim=c(-123,-110),ylim=c(33,50))
lines(blm_admin, col="gray")
lines(blm_states,col="darkgray")

par(mar=c(5.1,4.1,4.1,2.1))
xl <- 1;yb <- 1;xr <- 1.5;yt <- 2

plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/10),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/10),-1),
  col=cols_new(10)
)
q10<-round(quantile(as.numeric(col_df$c_val),seq(.1,1,.1)),1)

mtext(q10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
dev.off()


## ------------------- 4) Long Term Residuals, from forecast ------------------------------------
prod<-pred1$agb_m
cols1<-colorRampPalette(c("red", "yellow", "green"))


long_term_prod<-rowMeans(pred1$resids)
max_val<-round(max(long_term_prod),2)
min_val<-round(min(long_term_prod),2)
l<-seq(round(min_val,2),round(max_val,2),.01)

col_df<-data.frame(c_val=l,color.name=cols1(length(l)))
avg_agb_round<-as.data.frame(as.character(round(long_term_prod,2)));names(avg_agb_round)<-"avg_agb_round"
range(as.numeric(avg_agb_round$avg_agb_round))
avg_agb_round$long<-pred1$coord_df$long
avg_agb_round$lat<-pred1$coord_df$lat
as.character(col_df$c_val[!(as.character(col_df$c_val) %in% avg_agb_round$avg_agb_round)])
range(as.numeric(avg_agb_round$avg_agb_round))
# View(avg_agb_round)
avg_agb_round$avg_agb_round<-as.character(round(as.numeric(avg_agb_round$avg_agb_round),2))
col_df$c_val<-as.character(round(col_df$c_val,2))

long_term_prod2<-merge(avg_agb_round,col_df, by.x=as.character("avg_agb_round"),
                       by.y=as.character("c_val"), all.x=TRUE,all.y=TRUE)
range(as.numeric(long_term_prod2$avg_agb_round))

order_df<-long_term_prod2[order(as.numeric(long_term_prod2$avg_agb_round)),]
leg1<-as.data.frame(matrix(NA, nrow=3,ncol=2))

leg1[1,]<-c(order_df$avg_agb_round[1], order_df$color.name[1])
leg1[2,]<-c(order_df$avg_agb_round[round(nrow(order_df)*2/5,0)], order_df$color.name[round(nrow(order_df)*2/5,0)])
leg1[3,]<-c(order_df$avg_agb_round[round(nrow(order_df)*3/5,0)], order_df$color.name[round(nrow(order_df)*3/5,0)])
leg1[4,]<-c(order_df$avg_agb_round[round(nrow(order_df)*4/5,0)], order_df$color.name[round(nrow(order_df)*4/5,0)])
leg1[5,]<-c(order_df$avg_agb_round[(nrow(order_df))], order_df$color.name[(nrow(order_df))])
leg1
long_term_prod2<-na.omit(long_term_prod2)
sp<-long_term_prod2
sp$lat<-as.numeric(sp$lat)
sp$long<-as.numeric(sp$long)
coordinates(sp)<-~long+lat
sp@proj4string<-CRS("+init=epsg:4326")


png( "figures/summary_figures/mean_residuals_forecast_model.png")
layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))

plot('n',x=sp$long,y=sp$lat,xlab="", 
     ylab="", main="Mean residual 1986-2020 \n may forecast ", xlim=c(-123,-110),ylim=c(33,50),
     yaxt="none",xaxt="none")
print(abline(h = 30, col = "lightblue", lwd = 800))
print(abline(h = 50, col = "wheat", lwd = 200))
points(x=sp$long,y=sp$lat, pch=15, cex=.7,col=sp$color.name)

plot(blm_admin, col="lightgray",xlim=c(-123,-110),ylim=c(33,50), add=TRUE)
points(x=sp$long,y=sp$lat,
       pch=15, cex=.8,col=sp$color.name, xlim=c(-123,-110),ylim=c(33,50))
lines(blm_admin, col="gray")
lines(blm_states,col="darkgray")

par(mar=c(5.1,4.1,4.1,2.1))
xl <- 1;yb <- 1;xr <- 1.5;yt <- 2

par(mar=c(5.1,0.5,4.1,0.5))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/10),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/10),-1),
  col=cols1(10)
)
q10<-round(quantile(as.numeric(col_df$c_val),seq(0,.9,.1)),1)

mtext(q10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
dev.off()
