# ----------------------- packges, fncs----------------------
#need to factor in dividing by S probably....

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds")
meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

row_keeps<-meta_dat$row_keeps

prod_data_orig<-dat$prod[row_keeps,11:35]
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

sample_data<-as.data.frame(dat$samp[row_keeps])

fit1<-readRDS( "model_outputs/fuels_model_ppc.Rds")


traceplot(fit1, "alpha")
traceplot(fit1, "beta")
traceplot(fit1, "sig_o")
traceplot(fit1, "sig_p")
traceplot(fit1, "Fvec[33]")

#---------------------------------- check for retrieval, mixing, etc
print(summary(fit1)[[1]][seq(1,8,1),c(1,10)])


#mean and sd of params
alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
betaout<-rstan::extract(fit1, 'beta', permuted=F);beta<-apply(betaout, 3,c)
sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)

print(paste0("alpha mean: ", mean(alphaout),"alpha sd: ", sd(alphaout)))
print(paste0("beta mean: ", mean(betaout),"beta sd: ", sd(betaout)))
print(paste0("sig_o mean: ", mean(sig_oout),"sig_o sd: ", sd(sig_oout)))
print(paste0("sig_p mean: ", mean(sig_pout),"sig_p sd: ", sd(sig_pout)))
