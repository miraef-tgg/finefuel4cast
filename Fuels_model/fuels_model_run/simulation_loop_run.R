#This runs the final version of the Fuels Model and saves the output in the 'model_outputs' folder

# ----------------------- packgaes ----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds") 
meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

#additional data to run model only on locations and years with few missing data points
row_keeps<-meta_dat$row_keeps
year_length<-as.data.frame(meta_dat$year_length)
sample_data<-as.data.frame(dat$samp[row_keeps])

#standardizing data
prod_data_orig<-dat$prod[row_keeps,11:35]
fuel_data_orig<-dat$fine_fuel_obs[row_keeps,1:25]
fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

for(i in 1:nrow(fuel_data_z)){
  fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
}

fuel_data_z2<-replace(fuel_data_z, is.na(fuel_data_z), -1000) #it will be painfully obvious if model isn't skipping NA's


#backtransforming sig_o
sig_o_vals<-c(.01, seq(.1,3,.1))
sig_o_trans_vals<-vector()
for ( i in 1:length(sig_o_vals)){
  sig_o_trans_vals[i]<-sig_o_vals[i]*sd(fuel_data_orig[1,],na.rm=T)+mean(fuel_data_orig[1,],na.rm=T)
}


# plot(sig_o_vals, sig_o_trans_vals, ylim=c(0,8000), pch=19)

## run loop with high alpha

#sim params
alpha_sim=.5;beta_sim=.25;sig_p_sim=.6

alpha_vec<-vector(); alpha025_vec<-vector(); alpha975_vec<-vector()
beta_vec<-vector(); beta025_vec<-vector(); beta975_vec<-vector()
sig_o_vec<-vector(); sig_o025_vec<-vector(); sig_o975_vec<-vector()
sig_p_vec<-vector(); sig_p025_vec<-vector(); sig_p975_vec<-vector()
sim<-1

for ( sim in 1:length(sig_o_vals)) {
# for ( i in 1:2){
  sig_o_sim=sig_o_vals[sim]
  print(paste0( "sim: " , sig_o_vals[sim]," num: ", sim ))
  
  source(file="fuels_model_run/sim_loop_code.R")
  saveRDS(fit1,file= paste0("model_outputs/simulation/alpha/sig_o_",sig_o_vals[sim], ".rds" ))

}



## run loop with low beta

#sim params
alpha_sim=.1;beta_sim=.5;sig_p_sim=.6

alpha_vec<-vector(); alpha025_vec<-vector(); alpha975_vec<-vector()
beta_vec<-vector(); beta025_vec<-vector(); beta975_vec<-vector()
sig_o_vec<-vector(); sig_o025_vec<-vector(); sig_o975_vec<-vector()
sig_p_vec<-vector(); sig_p025_vec<-vector(); sig_p975_vec<-vector()
sim<-1

for ( sim in 1:length(sig_o_vals)) {
  # for ( i in 1:2){
  sig_o_sim=sig_o_vals[sim]
  print(paste0( "sim: " , sig_o_vals[sim]," num: ", sim ))
  
  source(file="fuels_model_run/sim_loop_code.R")
  saveRDS(fit1,file= paste0("model_outputs/simulation/beta/sig_o_",sig_o_vals[sim], ".rds" ))
  
}

