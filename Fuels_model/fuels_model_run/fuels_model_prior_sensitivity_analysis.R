# This runs >80 different priors and takes a very long time.
# the 'sig_f' isn't actually a prior--it's the initial condition uncertainty that is fixed at 0.8 in our code
# Outputs are saved in corresponding folders in ' in the 'model_outputs/sensitivity_analysis'


# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)


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

# ----------------------- stan code ----------------------
code<-readtext("stan_txts/fuels_model_sensitivity_code.txt")$text
# file.show("stan_txts/fuels_model_sensitivity_code.txt")

# ----------------------- combos  ----------------------

alpha_mu_vec<-seq(0,1,.1) 
alpha_sd_vec<-seq(.05,.5,.05)
beta_mu_vec<-seq(0,1,.1)
beta_sd_vec<-seq(.05,.5,.05)
sig_o_mu_vec<-seq(0,2,.1)
sig_o_sd_vec<-seq(.05,.5,.05)
sig_p_mu_vec<-seq(0,1,.1)
sig_p_sd_vec<-seq(.05,.5,.05)
sig_f_vec<-seq(0.1,2,.1)


var_list_str<-list("alpha_mu","alpha_sd", "beta_mu", "beta_sd","sig_o_mu","sig_o_sd", "sig_p_mu", "sig_p_sd", "sig_f")
var_list<-list(alpha_mu_vec,alpha_sd_vec, beta_mu_vec, beta_sd_vec,sig_o_mu_vec,sig_o_sd_vec, sig_p_mu_vec, sig_p_sd_vec, sig_f_vec)

# ----------------------- run model loop  ----------------------
for ( v in 1:9){
  #default values
  alpha_mu=rep(.25,100); alpha_sd=rep(.25,100)
  beta_mu=rep(.25,100) ;beta_sd=rep(.25,100)
  sig_o_mu=rep(.5,100) ;sig_o_sd=rep(.1,100)
  sig_p_mu=rep(0,100); sig_p_sd=rep(.25,100)
  sig_f=rep(.8,100);

  
  var_str=var_list_str[[v]]
  print(var_str);
  if (var_list_str[[v]]=="alpha_mu") {alpha_mu=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="alpha_sd") {alpha_sd=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="beta_mu") {beta_mu=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="beta_sd") {beta_sd=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="sig_o_mu") {sig_o_mu=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="sig_o_sd") {sig_o_sd=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="sig_p_mu") {sig_p_mu=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="sig_p_sd") {sig_p_sd=var_list[[v]]; var=var_list[[v]]}
  if (var_list_str[[v]]=="sig_f")   {sig_f=var_list[[v]]; var=var_list[[v]]}

  for ( i in 1:length(var)){
    
    print(paste0("/model_outputs/priors_sensitivity_analysis/", var_str, "/", var_str,"_", var[i], ".rds"))
    
    
    stan.data.real<-list("nLocs"=meta_dat$nLocs, "nYears"=meta_dat$nYears, "nMiss"=meta_dat$nMiss, "nOutputs"=meta_dat$nOutputs,
                         "P0"=meta_dat$P0,"P"=meta_dat$prod_data_vec,"first_val_vec"=as.data.frame(meta_dat$first_val_vec), "S"=sample_data,
                         "fuel_obs_index"=as.data.frame(meta_dat$fuels_obs_index), "prod_index"=as.data.frame(meta_dat$prod_index), "O"=fuel_data_z2,
                         "sig_o_mu"=sig_o_mu[i],"sig_o_sd"=sig_o_sd[i],
                         "sig_p_mu"=sig_p_mu[i], "sig_p_sd"=sig_p_sd[i],
                         "alpha_mu"=alpha_mu[i], "alpha_sd"=alpha_sd[i],
                         "beta_mu"=beta_mu[i], "beta_sd"=beta_sd[i],
                         "sig_f"=sig_f[i])

    fit1<-rstan::stan(model_code=code,data=stan.data.real,control=list(adapt_delta=.8),warmup=6000, iter=10000, chains=4,
                      save_warmup=F, refresh=0)

    saveRDS(fit1, paste0( "model_outputs/priors_sensitivity_analysis/", var_str, "/", var_str,"_",  var[i], ".rds"))
    print(summary(fit1)[[1]][c(1:4, 1168 ),c(1,10)])
    print(paste0("divergent iterations: " , sum(get_divergent_iterations(fit1))))
    print(paste0("time:",  sum(get_elapsed_time(fit1)[,1:2])/(60*60)))
    rm(fit1)
    gc()
  }
}
