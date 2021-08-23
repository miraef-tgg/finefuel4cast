#This script makes hindcasts from 1987-2020

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#fitted model
forecast_rds<-readRDS( "Prod_Forecast_model/prod_model_outputs/march_forecast_lm.rds")
forecast_mod<-forecast_rds$mod_fit
proc_err<-summary(forecast_mod)$sigma

#actual prod data from 1987-2020
prod_data<-as.data.frame(forecast_rds$agb_m)
names(prod_data)<-seq(1987,2020)

#forecasted/hindcasted data 1988-2021
prod_preds<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/prod_cast_1988_2021.csv")

(nLocs<-nrow(prod_preds))

# param uncertainty from monte carlo sampling 
march_forecast_param<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_param_2021.csv")
(param_err<-mean(apply(march_forecast_param,2,sd), na.rm=T))


# -------------- extracting information from Fuels Model ----------------

# want point estimates and distributions for each parameter

fit1<-readRDS("Fuels_model/model_outputs/fuels_model.rds")

alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
alpha_est<-mean(matrix(apply(alphaout,2,mean)))

betaout<-rstan::extract(fit1, 'beta', permuted=F);betaout<-apply(betaout, 3,c)
beta_est<-mean(matrix(apply(betaout,2,mean)))

sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
sig_o_est<-mean(matrix(apply(sig_oout,2,mean)))

sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)
sig_p_est<-mean(matrix(apply(sig_pout,2,mean)))

##------------------- Forecasting latent fuel of year 'y' by spinning up previous 10 years  ------------------------
# 'spin-up' latent fuel from y-10 years using Fuels Model param distributions and actual productivity data
# in year 'y' spin up using forecasted productivity data + forecast uncertainty


prod_start_yr<-1 #1987
latent_forecast_yr<-prod_start_yr+10 #1998

# matrix to hold spin-ups 
Fspin<-matrix(NA,nrow=nrow(prod_data), ncol=11) 

#set first val to prod 1987 
Fspin[,1]<-prod_data[,prod_start_yr]

#random iterations (out of 8000) to spin over
nIters<-100 #(hitting memory limits) 
random_iters<-sample(1:length(betaout),nIters)
Fspin_iters<-matrix(NA,nrow=nLocs*24,ncol=nIters) #holds all iterations as columns, each loc and year (1998-2021) as rows

length(seq(1998,2021,1))

prod_data_subset<-prod_data[,prod_start_yr:(prod_start_yr+10)] #1987:1997
prod_preds_subset<-prod_preds[,latent_forecast_yr]

head(prod_data_subset) #1987-1997
(prod_preds_subset) #1998
# for (i in 1:nIters){'
i<-1
count=1

#subset forecasted data to 1 year
#subset prod_data to 10 years

for ( i in 1:nIters){
  count=1
  for (prod_start_yr in 1:24){
    latent_forecast_yr<-prod_start_yr+10 #starting at 1998
    prod_data_subset<-prod_data[,prod_start_yr:(prod_start_yr+10)] #subset prod_data to 10 years
    prod_preds_subset<-prod_preds[,latent_forecast_yr] #subset forecasted data to 1 year
    
    for ( cur_yr in 2:11){  #loop through real data first 10 yrs
      if(cur_yr!=11){ 
        iter<-random_iters[i]
        prev_yr<-cur_yr-1
        yr_name<-1986+cur_yr
        Fspin[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*prod_data_subset[,cur_yr] + rnorm(nLocs,0,sig_pout[iter])
        names(Fspin)[prev_yr]<-yr_name
        
      }
      
      if(cur_yr==11){ #last year, spin up on forecasted data, add to data frame
        (iter<-random_iters[i])
        prev_yr<-cur_yr-1
        yr_name<-1986+cur_yr
        # Fspin[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*prod_preds_subset #check without error
        Fspin[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*(prod_preds_subset+rnorm(nLocs, 0, proc_err)) + rnorm(nLocs,0,sig_pout[iter])
        Fspin_iters[(nLocs*(count-1)+1):(count*nLocs),i]<-Fspin[,11]
      }
    }   
    count=count+1 
  }
}




# check; rows = locations and years, columns = iterations
# View(Fspin_iters[(nLocs-30):(nLocs+30),])
#nrow(Fspin_iters)/nLocs


write.csv(Fspin_iters, "Fine_Fuels_Forecast/output_data/latent_forecast_distrib.csv", row.names=F)


##---------------------------------- changing into smaller, more intuitive format --------------------

#change very large Fspin_iters into 80% confidence interval in nLocs*nYears matrices
Fspin_iters_90<-matrix(NA,nrow=nLocs,ncol=24)
Fspin_iters_10<-matrix(NA,nrow=nLocs,ncol=24)
Fspin_iters_est<-matrix(NA,nrow=nLocs,ncol=24)


for ( y in 1:24){
  cur_Fspin<-as.data.frame(Fspin_iters[((y-1)*nLocs+1):(y*nLocs),]) #just one year of locations, all iterations
  Fspin_iters_90[,y]<-apply(cur_Fspin,1,quantile, probs=.9,na.rm=T)
  Fspin_iters_10[,y]<-apply(cur_Fspin,1,quantile, probs=.1,na.rm=T)
  Fspin_iters_est[,y]<-apply(cur_Fspin,1,mean,na.rm=T)
  
}

dim(cur_Fspin) 
dim(Fspin_iters_90)

# backtransform: run first chunk of march_all_lmm_predictions_z.R
model_df2<-read.csv("Prod_Forecast_model/gee_4cast_data/model_csvs/march_all_model_csv.csv")
agb<-model_df2$agb
agb_m<-matrix(NA,nrow=nLocs,ncol=34)

for (i in 1:34){
  agb_m[1:nLocs,i]<-agb[(nLocs*(i-1)+1):(i*nLocs)]
  # print(paste0((nLocs*(i-1)+1), " : " , (i*nLocs)))
}

long_term_sd<-apply(agb_m,1,sd)
long_term_mean<-apply(agb_m,1,mean)


Fspin_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_90_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_10_raw<-matrix(NA,nrow=nLocs,ncol=24)
for ( i in 1:ncol(Fspin_iters_est)){
  Fspin_raw[,i]<-(Fspin_iters_est[,i] * long_term_sd)+long_term_mean
  Fspin_90_raw[,i]<-(Fspin_iters_90[,i] * long_term_sd)+long_term_mean
  Fspin_10_raw[,i]<-(Fspin_iters_10[,i] * long_term_sd)+long_term_mean
}

# change to % above/below normal
Fspin_perc<-matrix(NA,nrow=nLocs,ncol=24); Fspin_90_perc<-matrix(NA,nrow=nLocs,ncol=24); Fspin_10_perc<-matrix(NA,nrow=nLocs,ncol=24)

for ( i in 1:ncol(Fspin_iters_est)){
  Fspin_perc[,i]<-(Fspin_raw[,i]-model_df2$avg_agb[1:nLocs])/model_df2$avg_agb[1:nLocs]*100
  Fspin_90_perc[,i]<-(Fspin_90_raw[,i]-model_df2$avg_agb[1:nLocs])/model_df2$avg_agb[1:nLocs]*100
  Fspin_10_perc[,i]<-(Fspin_10_raw[,i]-model_df2$avg_agb[1:nLocs])/model_df2$avg_agb[1:nLocs]*100
}

range(Fspin_perc, na.rm=T)

colMeans(Fspin_10_perc)
colMeans(Fspin_90_perc)

nYears<-24
plot(x=seq(1998,2021), y=colMeans(Fspin_10_perc), col="red", lwd=2, lty=2, type="l", ylim=c(-50,50),
     main = "Percent above long-term normal", xlab="year", ylab="% above long term average", cex.main=2)
lines(x=seq(1998,2021), y=colMeans(Fspin_90_perc), col="red", lty=2, lwd=2)
lines(x=seq(1998,2021), y=colMeans(Fspin_perc), col="red", lwd=3)
lines(x=seq(1998,2021), y=rep(0,24), lty=2)
legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)

##by district::
districts<-read.csv("Fine_Fuels_forecast/coords.csv")
districts$long_lat<-paste0(substr(districts$long,1,7), "_", substr(districts$lat,1,7))
loc_keeps<-read.csv("Prod_Forecast_model/prod_model_outputs/loc_keeps.csv")
coords<-as.data.frame(loc_keeps)
coords$long_lat<-paste0(substr(coords$long,1,7), "_", substr(coords$lat,1,7))


dim(coords); dim(districts)
coords_dist<-plyr::join(coords, districts, by ="long_lat", type="left")
dim(coords_dist)
head(coords_dist)
coords_dist$dupes<-duplicated(paste0(coords_dist$long, coords_dist$lat))
coords_dist<-subset(coords_dist, coords_dist$dupes==F)
dim(coords_dist)

Fspin_iters_10_sp<-as.data.frame(cbind(Fspin_10_perc, coords_dist$long, coords_dist$lat, coords_dist$PARENT_N))
names(Fspin_iters_10_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), "long", "lat", "district")
Fspin_iters_90_sp<-as.data.frame(cbind(Fspin_90_perc, coords_dist$long, coords_dist$lat, coords_dist$PARENT_N))
names(Fspin_iters_90_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), "long", "lat", "district")
Fspin_iters_est_sp<-as.data.frame(cbind(Fspin_perc, coords_dist$long, coords_dist$lat, coords_dist$PARENT_N))
names(Fspin_iters_est_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), "long", "lat", "district")

#this seems potentially useful too
latent_forecast_perc<-list(Fspin_iters_10_sp, Fspin_iters_90_sp,Fspin_iters_est_sp)
# names(latent_forecast_perc)<-c("perc_above_avg_10CI", "perc_above_avg_10CI","perc_above_avg_10CI")
names(fuelcast_perc)<-c("CI10", "CI90", "mean")

saveRDS(latent_forecast_perc, "Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds")
