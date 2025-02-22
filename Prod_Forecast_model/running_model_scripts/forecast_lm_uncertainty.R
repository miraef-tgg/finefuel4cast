# this script calculates parameter uncertainty from the forecast_lm model using monte carlo sampling


#make point estimates for hindcast data as well 
## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


model_df_orig<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
model_df<-model_df_orig

#if nIters isn't definted by user
if ( !exists(deparse(substitute(nIters))))(nIters<-100 )

#mod1: linear forecast
mod1_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                 prev_pfg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm))


nLocs<-nrow(mod1_data)

dim(mod1_data)

save_loc<-"/figures/march_vis/z_forecast/"
save_name<-"z_march_forecast_lm"
(lm_coeffs<-read.csv( "prod_model_outputs/march_forecast_lm_coeffs.csv"))
forecast_mod<-readRDS( "prod_model_outputs/march_forecast_lm.rds")
forecast_mod<-forecast_mod$mod_fit

# # ----------------------- no uncertainty  -------------------------
#use lm_coeffs for point estimates of coefficients
lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])
march_forecast_point_est<-matrix(NA,nrow=nIters,ncol=nLocs)

march_forecast_point_est<-lm_coeffs[lm_coeffs[,1]=="int",2]+
  lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*mod1_data$prev_z_agb +
  lm_coeffs[lm_coeffs[,1]=="z_pr",2]*mod1_data$z_pr +
  lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*mod1_data$z_tmmx +
  lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*mod1_data$z_vpd +
  lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*mod1_data$z_ndvi +
  lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*mod1_data$pr_frac +
  lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*mod1_data$prev_pfg_frac +
  lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*mod1_data$z_ssm +
  lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*mod1_data$z_bulk_dens +
  lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*mod1_data$z_pr_z_ssm 

march_forecast_point_est[1:10]


write.csv(march_forecast_point_est,"prod_model_outputs/forecast2021/march_forecast_point_est_2021.csv",row.names = F )



# # ----------------------- param uncertainty  -------------------------
#use vcov() to get covariance mnatrix of coeff and rmvnorm to sample

march_forecast_param<-matrix(NA,nrow=nIters,ncol=nLocs)
forecast_iter<-vector()
mu = coef(forecast_mod)
sigma = vcov(forecast_mod)
n = matrix(NA,nIters,34)   # storage for all simulations
coef_samples = rmvnorm(nIters,mean=mu,sigma=sigma)  # sample values of coefficients
coef_samples<-as.data.frame(coef_samples)
names(coef_samples)<-names(mu)

for ( i in 1:nIters){
forecast_iter<-coef_samples[i,names(coef_samples)=="(Intercept)"]+
  coef_samples[i,names(coef_samples)=="prev_z_agb"]*mod1_data$prev_z_agb +
  coef_samples[i,names(coef_samples)=="z_pr"]*mod1_data$z_pr +
  coef_samples[i,names(coef_samples)=="z_tmmx"]*mod1_data$z_tmmx +
  coef_samples[i,names(coef_samples)=="z_vpd"]*mod1_data$z_vpd +
  coef_samples[i,names(coef_samples)=="z_ndvi"]*mod1_data$z_ndvi +
  coef_samples[i,names(coef_samples)=="pr_frac"]*mod1_data$pr_frac +
  coef_samples[i,names(coef_samples)=="prev_pfg_frac"]*mod1_data$prev_pfg_frac +
  coef_samples[i,names(coef_samples)=="z_ssm"]*mod1_data$z_ssm +
  coef_samples[i,names(coef_samples)=="z_bulk_dens"]*mod1_data$z_bulk_dens +
  coef_samples[i,names(coef_samples)=="z_pr_z_ssm"]*mod1_data$z_pr_z_ssm
march_forecast_param[i,]<-forecast_iter
}

march_forecast_param[1:10,1:10]
mean(apply(march_forecast_param,2,sd), na.rm=T) #point estimate of param uncertainty

# monte carlo sampling param uncertainty
write.csv(march_forecast_param, "prod_model_outputs/forecast2021/march_forecast_param_2021.csv",row.names = F )


# # ----------------------- proc uncertainty  -------------------------
#use lm_coeffs for point estimates of coefficients, add proc err with rnorm()
march_forecast_proc<-matrix(NA,nrow=nIters,ncol=nLocs)
Y_pred<-forecast_mod$fitted.values
(proc_err<-summary(forecast_mod)$sigma)
results<-readRDS( "prod_model_outputs/march_forecast_lm.rds")
sd(results$resids)
lm_coeffs[lm_coeffs[,1]=="int",2]

for ( i in 1:nIters){
  forecast_iter<-lm_coeffs[lm_coeffs[,1]=="int",2]+
    lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*mod1_data$prev_z_agb +
    lm_coeffs[lm_coeffs[,1]=="z_pr",2]*mod1_data$z_pr +
    lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*mod1_data$z_tmmx +
    lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*mod1_data$z_vpd +
    lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*mod1_data$z_ndvi +
    lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*mod1_data$pr_frac +
    lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*mod1_data$prev_pfg_frac +
    lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*mod1_data$z_ssm +
    lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*mod1_data$z_bulk_dens +
    lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*mod1_data$z_pr_z_ssm + 
    rnorm(nLocs, 0, proc_err)
    march_forecast_proc[i,]<-forecast_iter
}

march_forecast_proc[1:10,1:10]


write.csv(march_forecast_proc,  "prod_model_outputs/forecast2021/march_forecast_proc_2021.csv",row.names = F )

mean(apply(march_forecast_param,2,sd), na.rm=T) #point estimate of param uncertainty
mean(apply(march_forecast_proc,2,sd), na.rm=T) #point estimate of proc uncertainty

