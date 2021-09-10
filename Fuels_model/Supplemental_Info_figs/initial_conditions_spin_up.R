# We wanted to make sure the way we run the Fuels Model ehis makes three plots

## ------------------------- data/pcks -------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, RColorBrewer, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


#model
mod<-readRDS("model_outputs/fuels_model.rds")

#data
dat<-readRDS("fuels_model_data/fuels_model_data.rds")
meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

row_keeps<-meta_dat$row_keeps

prod_data_orig<-dat$prod[row_keeps,1:35]
fuel_data_orig<-dat$fine_fuel_obs[row_keeps,1:25]
fuel_data_dev<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_dev<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))
fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

for(i in 1:nrow(fuel_data_z)){
  fuel_data_dev[i,]<-fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_dev[i,]<-prod_data_orig[i,]-mean(prod_data_orig[i,])
  fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
}

prod_data_z[1:10,]

## ------------------------- parameters (point estimates) -------------------------------------------

#extract
sig_p<-rstan::extract(mod, 'sig_p', permuted=F);mean_sig_p<-mean(apply(sig_p, 3,c));sd_sig_p<-sd(apply(sig_p, 3,c))
beta<-rstan::extract(mod, 'beta', permuted=F);beta<-mean(apply(beta, 3,c))
alpha<-rstan::extract(mod, 'alpha', permuted=F);alpha<-mean(apply(alpha, 3,c))
sig_o<-rstan::extract(mod, 'sig_o', permuted=F);sig_o<-mean(apply(sig_o, 3,c))


## ------------------------- simulating 1000 yrs of productivity data -------------------------------------------

#simulate more yrs of prod from ~N(rowmean, rowsd)
prod_sim<-matrix(NA, nrow=nrow(prod_data_orig), ncol=1000)
prod_sim[,1:35]<-prod_data_orig
for ( i in 1:nrow(prod_data_orig)){
  prod_sim[i,35:1000]<-rnorm((1001-35),mean=mean(prod_data_orig[i,]),sd(prod_data_orig[i,]))
}
#look
# plot(prod_sim[1,], type="l")
# prod_sim[1:5,30:40]

rand<-sample.int(148,size=5)
#look
par(mfrow=c(1,1))
sim_length<-200
plot("n", main="Simulated prod: \n years 1-34 are real data \n years 35+ are simulations based on years 1-34", 
     xlim=c(0,sim_length), ylim=c(0,2000))
cols<-brewer.pal(n=5,"Spectral")
for(i in 1:length(rand)){
  lines((prod_sim[rand[i],1:sim_length]),type="l", col=cols[i])
}
## ------------------------- Simulation 1000 years of fuel data based on fuels model -------------------------------------------
nLocs<-148

prod_start<-rowMeans(prod_data_orig)
nLocs<-length(prod_start)
yrs<-1000
fuel_sim<-matrix(NA,nrow=nLocs,ncol=yrs)
fuel_sim[,1]<-prod_start
(fuel_sim[1:10,1:30])

for (i in 2:yrs){
  fuel_sim[,i]<-rnorm(nLocs, (alpha*fuel_sim[,(i-1)]+beta*prod_sim[,i]),mean_sig_p)
}


#look
par(mfrow=c(1,1))
sim_length<-200
plot("n", main="Simulated fuel: \n a*fuel[,t-1] + b*prod[,2] + proc_err", 
     xlim=c(0,sim_length), ylim=c(0,1000))
cols<-brewer.pal(n=5,"Spectral")
for(i in 1:length(rand)){
  lines((fuel_sim[rand[i],1:sim_length]),type="l", col=cols[i])
}


## ------------------------- Independence from original value? -------------------------------------------

# How long til it is independent of original value?
# autocorrelation definitely has disappeared by 10 years

acf_df<-data.frame(matrix(NA, nrow=10,ncol=22))
tfuel<-t(fuel_sim)
for( i in 1:nrow(fuel_sim)){
  # a<-(acf(tfuel[,i],lag.max = 15,plot=FALSE)$acf)
  acf_df[i,]<-t(as.vector(acf((tfuel[,i]),lag.max = 21,plot=FALSE)$acf))
}


#look
plot(x=seq(1,20,1), y=acf_df[1,1:20], ylim=c(-.1,1),type="l", xlab="lag year", ylab="pacf",
     main="autocorr coef of simulated fuels data over time")
for(i in 1:nrow(acf_df)){
  lines(x=seq(1,20,1), y=acf_df[i,1:20],type="l", col=cols[i])
}
abline(0.05,0,.05, lty=2)

#save
png("Supplemental_Info_figs/spin_up_test_figs/autocorrelation_sim_fuels.png")
plot(x=seq(1,20,1), y=acf_df[1,1:20], ylim=c(-.1,1),type="l", xlab="lag year", ylab="pacf",
     main="autocorr coef of simulated fuels data over time")
for(i in 1:nrow(acf_df)){
  lines(x=seq(1,20,1), y=acf_df[i,1:20],type="l", col=cols[i])
}
abline(0.05,0,.05, lty=2)
dev.off()

## ------------------------- sensitivity to init value -------------------------------------------
loc<-sample.int(nrow(fuel_sim), size=1)
#if you start at diff values, what happens?

prod_start<-mean(prod_data_orig[loc,])
sim_length<-100

fuel_sens_sim<-matrix(NA,nrow=12,ncol=100)
fuel_sens_sim[1,1]<-prod_start
fuel_sens_sim[2:12,1]<-seq(from=0, to=1000, length.out=11)
yrs<-100
head(fuel_sens_sim[1:5,1:5])
cols<-brewer.pal(n=11,"Spectral")


for (i in 2:yrs){
  fuel_sens_sim[,i]<-rnorm(12, (alpha*fuel_sens_sim[,(i-1)]+beta*prod_sim[loc,i]),mean_sig_p)
}
head(fuel_sens_sim[1:5,1:5])

png("Supplemental_Info_figs/spin_up_test_figs/sensitivity_to_start_val.png")
sim_length<-10
plot(x=seq(1,sim_length,1),y=(fuel_sens_sim[1,1:sim_length]),type="l",lwd=2, col="black",
     main="Simulated fuel: 0-10 start val", xlim=c(0,sim_length), ylim=c(0,1000))
for(i in 2:nrow(fuel_sens_sim)){
  lines(x=seq(1,sim_length,1),y=(fuel_sens_sim[i,1:sim_length]),type="l", col=cols[i], lwd=2)
}
lines(x=seq(1,sim_length,1),y=(fuel_sens_sim[1,1:sim_length]), lty=2 )
points(x=seq(1,sim_length,1),y=(fuel_sens_sim[1,1:sim_length]), pch=19, cex=1)
legend("topleft", legend="'real'  start val", lty=1,pch=19)

dev.off()




## ------------------------- trajectory bounds -------------------------------------------
#given hi/lo estimates of alpha/beta/sig_p, what are range of values that fuel can take?
loc<-sample.int(nrow(fuel_sim), size=1)

sig_p10<-rstan::extract(mod, 'sig_p', permuted=F);mean_sig_p<-mean(apply(sig_p, 3,c));sd_sig_p<-sd(apply(sig_p, 3,c))

beta<-rstan::extract(mod, 'beta', permuted=F);
beta10<-mean(apply(beta, 3,quantile, prob=.1))
beta20<-mean(apply(beta, 3,quantile, prob=.2))
beta80<-mean(apply(beta, 3,quantile, prob=.8))
beta90<-mean(apply(beta, 3,quantile, prob=.9))

alpha<-rstan::extract(mod, 'alpha', permuted=F);
alpha10<-mean(apply(alpha, 3,quantile, prob=.1))
alpha20<-mean(apply(alpha, 3,quantile, prob=.2))
alpha80<-mean(apply(alpha, 3,quantile, prob=.8))
alpha90<-mean(apply(alpha, 3,quantile, prob=.9))

sig_p<-rstan::extract(mod, 'sig_p', permuted=F);
sig_p10<-mean(apply(sig_p, 3,quantile, prob=.1))
sig_p90<-mean(apply(sig_p, 3,quantile, prob=.9))

prod_start<-rowMeans(prod_data_z)
nLocs<-length(prod_start)

traj_bounds<-matrix(NA,nrow=13,ncol=1000)
traj_bounds[,1]<-mean(prod_data_orig[loc,])
traj_bounds[,1:5]
yrs<-1000

for (i in 2:yrs){
  traj_bounds[1,i]<-rnorm(1, (alpha*traj_bounds[1,(i-1)]+beta*prod_sim[loc,i]),mean_sig_p)
  #alpha_prob + beta_prob =1
  traj_bounds[2,i]<-rnorm(1, (alpha10*traj_bounds[2,(i-1)]+beta90*prod_sim[loc,i]),sig_p90)
  traj_bounds[3,i]<-rnorm(1, (alpha20*traj_bounds[3,(i-1)]+beta80*prod_sim[loc,i]),sig_p90)
  traj_bounds[4,i]<-rnorm(1, (alpha80*traj_bounds[4,(i-1)]+beta20*prod_sim[loc,i]),sig_p90)
  traj_bounds[5,i]<-rnorm(1, (alpha90*traj_bounds[5,(i-1)]+beta10*prod_sim[loc,i]),sig_p90)
  #low
  traj_bounds[6,i]<-rnorm(1, (alpha10*traj_bounds[6,(i-1)]+beta10*prod_sim[loc,i]),sig_p90)
  traj_bounds[7,i]<-rnorm(1, (alpha20*traj_bounds[7,(i-1)]+beta20*prod_sim[loc,i]),sig_p90)
  #hi
  traj_bounds[8,i]<-rnorm(1, (alpha80*traj_bounds[8,(i-1)]+beta80*prod_sim[loc,i]),sig_p90)
  traj_bounds[9,i]<-rnorm(1, (alpha90*traj_bounds[9,(i-1)]+beta90*prod_sim[loc,i]),sig_p90)
  #lo sig_p
  traj_bounds[10,i]<-rnorm(1, (alpha90*traj_bounds[5,(i-1)]+beta10*prod_sim[loc,i]),sig_p10)
  traj_bounds[11,i]<-rnorm(1, (alpha10*traj_bounds[5,(i-1)]+beta90*prod_sim[loc,i]),sig_p10)
  traj_bounds[12,i]<-rnorm(1, (alpha90*traj_bounds[5,(i-1)]+beta90*prod_sim[loc,i]),sig_p10)
  traj_bounds[13,i]<-rnorm(1, (alpha10*traj_bounds[5,(i-1)]+beta10*prod_sim[loc,i]),sig_p10)
  
}

cols<-brewer.pal(n=7,"Spectral")
png("Supplemental_Info_figs/spin_up_test_figs/possible_tranjectors.png")

plot(traj_bounds[1,1:yrs],type="l", col=cols[i], ylim=c(-1,1000), main="possible trajectories of one location's simulated fuel values")
for(i in 1:nrow(traj_bounds)){
  lines((traj_bounds[12,1:yrs]),type="l", col="black")
  lines((traj_bounds[13,1:yrs]),type="l", col="black")
  
  lines((traj_bounds[1,1:yrs]),type="l", col=cols[1])
  lines((traj_bounds[2,1:yrs]),type="l", col=cols[2])
  lines((traj_bounds[3,1:yrs]),type="l", col=cols[3])
  lines((traj_bounds[4,1:yrs]),type="l", col=cols[4])
  lines((traj_bounds[5,1:yrs]),type="l", col=cols[5])
  
  lines((traj_bounds[6,1:yrs]),type="l", col="red")
  lines((traj_bounds[7,1:yrs]),type="l", col="red")
  lines((traj_bounds[8,1:yrs]),type="l", col="green")
  lines((traj_bounds[9,1:yrs]),type="l", col="green")
  
  
  lines((traj_bounds[10,1:yrs]),type="l", col=cols[6])
  lines((traj_bounds[11,1:yrs]),type="l", col=cols[7])
  
}
legend("topleft", legend=c("hi alpha, hi beta, hi sig_p", "lo alpha, lo beta, hi sig_p",  "hi alpha or hi beta, lo sig_p","p(alpha)+p(beta)=1, hi sig_p"),
       col=c("green", "red",  "black",cols[7]), lty=1)

dev.off()

