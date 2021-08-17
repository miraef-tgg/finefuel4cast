#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# fnc that returns predictions for t+1 using model's a/b point estimates, sig_o / sig_p iterations, and input prod/fuel data
# returns in WIDE format; nrow=nIters, ncol=nPredictions
# fuel_est[,cur_yr]<-alphaout[i]*  fuel_est[,prev_yr]+ betaout[i]* prod_data[,cur_yr] +   rnorm(1,0, sig_pout[i]) + rnorm(1,0, sig_oout[i]/mean_samp)

#---------- function -------------
par(mfrow=c(1,1))


make_preds_unscaled_wide<-function(mod,fuel_data,prod_data, nIters, nOutputs,sorted_prod_m_match, 
                                   param_uncert=TRUE,
                                   proc_uncert=TRUE,
                                   obs_uncert=TRUE,
                                   mean_samp=1,
                                   prod_start_val_vec,
                                   sig_f=.1,
                                   prod_m_match){
  # mod<-fit1; prod_data=prod_data; fuel_dat=fuel_data;nIters=200;nOutputs=1165;sorted_prod_m_match=prod_m_match;
  # param_uncert=FALSE;proc_uncert=FALSE;obs_uncert=FALSE
  
  #make matrix to store results
  fpred_results<-matrix(NA,nrow=nIters,ncol=nOutputs)
  #extract param iterations from model
  alphaout<-(rstan::extract(mod, 'alpha', permuted=F))
  betaout<-(rstan::extract(mod, 'beta', permuted=F))
  sig_pout<-(rstan::extract(mod, 'sig_p', permuted=F))
  sig_oout<-(rstan::extract(mod, 'sig_o', permuted=F))
  nLocs=nrow(fuel_data);  nyears=ncol(fuel_data)

  #if uncert = FALSE, set to zero or point mean
  if (param_uncert==FALSE){
    alphaout<-rep(mean(alphaout), length(alphaout))
    betaout<-rep(mean(betaout), length(betaout))
  }
  if (proc_uncert==FALSE){
    sig_pout<-rep(0, length(alphaout))
  }
  if (obs_uncert==FALSE){
    sig_oout<-rep(0, length(alphaout))
  }
  
  fuel_est<-matrix(NA,nrow=nLocs ,ncol=nyears)
  fuel_est[,1]<-rnorm(nLocs, as.numeric(prod_start_val_vec), sig_f) 
  #outer loop: use one iteration of a/b/sig_o/sig_p to make predictions n+1 in matrix
  
  for(i in 1:nIters){
    #inner loop: make predictions for all locations, 1 year at a time
      for ( cur_yr in 2:(nyears)){
        prev_yr<-cur_yr-1
        if (cur_yr==2){
        fuel_est[,cur_yr]<-alphaout[i]*  fuel_est[,prev_yr]+ betaout[i]* prod_data[,cur_yr] + 
          rnorm(1,0, sig_pout[i]) + rnorm(1,0, sig_oout[i]/sqrt(mean_samp))
        }
        if (cur_yr!=2){
          fuel_est[,cur_yr]<-alphaout[i]*  fuel_est[,prev_yr]+ betaout[i]* prod_data[,cur_yr] + 
            rnorm(1,0, sig_pout[i]) + rnorm(1,0, sig_oout[i]/sqrt(mean_samp))
        }
      }
    
    #put predictions for all locs/years from one iteration into a vector
    f_preds_vec<-as.data.frame(as.vector(t(as.matrix(fuel_est))))
    f_preds_vec$loc_yr<-paste0(rep(1:nLocs,each=nyears), "_", rep(1:nyears,nLocs))
    f_preds_vec$match <- f_preds_vec$loc_yr %in% sorted_prod_m_match$loc_yr
    f_preds_vec_match<-f_preds_vec[f_preds_vec[,3],]
    f_preds_vec<-f_preds_vec_match[,1]
    fpred_results[i,]<-f_preds_vec
    }
  return (fpred_results)
}


perc_true<-function(dat_col){
  sum(dat_col==TRUE,na.rm=TRUE)/sum(!(is.na(dat_col)))
}



plot_coverage<-function(mod=fit3, q_upper=.95, q_lower=.05,
                        nIters=1000, mod_name="",fuel_dat=fuel_data, prod_dat=prod_data,
                        prod_start_val_vec, mean_samp=1, sig_f_par=.1,prod_m_match,show_plot=F){
  # mod=fit1; q_upper=.95; q_lower=.05;sig_f_par=F;
  # nIters=1000; mod_name="";fuel_dat=fuel_data_z; prod_dat=prod_data_z;
  # prod_val_vec=rep(0,148)
  #  
fuel_data_vec<-as.vector(t(as.matrix(fuel_dat)))
fuel_data_vec<-as.data.frame(fuel_data_vec)
nLocs<-148;nYears<-25
prod_m_match<-prod_m_match[order(prod_m_match$yr, decreasing=FALSE),]
fuel_data_vec$loc_yr<-paste0(rep(1:nLocs,each=nYears), "_", rep(1:nYears,nLocs))
fuel_data_vec$match <- fuel_data_vec$loc_yr %in% prod_m_match$loc_yr
fuel_data_vec_match<-fuel_data_vec[fuel_data_vec[,3],]
fuel_data_vec<-fuel_data_vec_match$fuel_data_vec

fpred_par_uncert3<-make_preds_unscaled_wide(mod, prod_data=prod_dat, fuel_data=fuel_dat,nIters=nIters,nOutputs=1165, 
                                            prod_start_val_vec=prod_start_val_vec, sorted_prod_m_match = prod_m_match,
                                            proc_uncert=FALSE, param_uncert=TRUE, obs_uncert=FALSE, mean_samp = mean_samp, sig_f=sig_f_par)

fpred_proc_uncert3<-make_preds_unscaled_wide(mod, prod_data=prod_dat, fuel_data=fuel_dat,nIters=nIters,nOutputs=1165, 
                                             prod_start_val_vec=prod_start_val_vec, sorted_prod_m_match = prod_m_match,
                                             proc_uncert=TRUE, param_uncert=FALSE, obs_uncert=FALSE, mean_samp = mean_samp,sig_f=sig_f_par)

fpred_obs_uncert3<-make_preds_unscaled_wide(mod, prod_data=prod_dat, fuel_data=fuel_dat,nIters=nIters,nOutputs=1165,
                                            prod_start_val_vec=prod_start_val_vec, sorted_prod_m_match = prod_m_match,
                                            proc_uncert=FALSE, param_uncert=FALSE, obs_uncert=TRUE, mean_samp = mean_samp,sig_f=sig_f_par)

fpred_all_uncert3<-make_preds_unscaled_wide(mod, prod_data=prod_dat, fuel_data=fuel_dat,nIters=nIters,nOutputs=1165,
                                            prod_start_val_vec=prod_start_val_vec, sorted_prod_m_match = prod_m_match,
                                            proc_uncert=TRUE, param_uncert=TRUE, obs_uncert=TRUE, mean_samp = mean_samp,sig_f=sig_f_par)

#make df
cov_df_stan_mod<-as.data.frame(fuel_data_vec)
names(cov_df_stan_mod)<-"obs"

cov_df_stan_mod$par_err90<-apply(fpred_par_uncert3,2,quantile, q_upper)
cov_df_stan_mod$par_err10<-apply(fpred_par_uncert3,2,quantile, q_lower)
cov_df_stan_mod$obs_err90<-apply(fpred_obs_uncert3,2,quantile, q_upper)
cov_df_stan_mod$obs_err10<-apply(fpred_obs_uncert3,2,quantile, q_lower)
cov_df_stan_mod$proc_err90<-apply(fpred_proc_uncert3,2,quantile, q_upper)
cov_df_stan_mod$proc_err10<-apply(fpred_proc_uncert3,2,quantile, q_lower)
cov_df_stan_mod$all_err90<-apply(fpred_all_uncert3,2,quantile, q_upper)
cov_df_stan_mod$all_err10<-apply(fpred_all_uncert3,2,quantile, q_lower)

cov_df_stan_mod$par_cov<-(cov_df_stan_mod$obs <cov_df_stan_mod$par_err90 & cov_df_stan_mod$obs > cov_df_stan_mod$par_err10)
cov_df_stan_mod$obs_cov<-(cov_df_stan_mod$obs <cov_df_stan_mod$obs_err90 & cov_df_stan_mod$obs > cov_df_stan_mod$obs_err10)
cov_df_stan_mod$proc_cov<-(cov_df_stan_mod$obs <cov_df_stan_mod$proc_err90 & cov_df_stan_mod$obs > cov_df_stan_mod$proc_err10)
cov_df_stan_mod$all_cov<-(cov_df_stan_mod$obs <cov_df_stan_mod$all_err90 & cov_df_stan_mod$obs > cov_df_stan_mod$all_err10)

##comp_cov

coverage_df<-as.data.frame(cbind(cov_df_stan_mod$par_cov,cov_df_stan_mod$obs_cov,
                                  cov_df_stan_mod$proc_cov, cov_df_stan_mod$all_cov
                                  ))

names(coverage_df)<-c("stan_par_cov", "stan_obs_cov", "stan_proc_cov", "stan_all_cov")
# return(coverage_df)
cov_df <- coverage_df %>%
  mutate("param_err"= perc_true(stan_par_cov),"obs_err"= perc_true(stan_obs_cov),
         "proc_err"= perc_true(stan_proc_cov),"total_err"= perc_true(stan_all_cov)
         )

summary_coverage<-t((cov_df[1,5:8]))
sum_df<-as.data.frame(matrix(NA,nrow=4,ncol=0))
sum_df$CI90<-as.vector(summary_coverage)
sum_df$name<-paste0(names(cov_df)[5:8])
print(summary_coverage)
if(show_plot){
barplot(height=sum_df$CI90,cex.names=.8, names.arg=sum_df$name, 
        ylim=c(0,1), main = paste0(q_upper, "-",q_lower, " coverage \n", mod_name),
        xlab="Source(s) of uncertainty included", ylab="% of latent fuel at t+1 within CI of observed data")
abline(a=(q_upper-q_lower), b=0, h=(q_upper-q_lower), lty=2)
}
return(sum_df)
                     
}



# ----------------------- test ----------------------

# dat<-readRDS("fuels_model_data/fuels_model_data.rds")
# meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")
# 
# row_keeps<-meta_dat$row_keeps
# 
# prod_data_orig<-dat$prod[row_keeps,11:35]
# fuel_data_orig<-dat$fine_fuel_obs[row_keeps,1:25]
# fuel_data_dev<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
# prod_data_dev<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))
# fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
# prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))
# 
# for(i in 1:nrow(fuel_data_z)){
#   fuel_data_dev[i,]<-fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE)
#   prod_data_dev[i,]<-prod_data_orig[i,]-mean(prod_data_orig[i,])
#   fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,],na.rm=TRUE)
#   prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
# }
# 
# sample_data<-as.data.frame(dat$samp[row_keeps])
# mean_samp<-mean(sample_data[,1])
# fit1<-readRDS( "model_outputs/fuels_model.rds")
# 
# uncertainty<-plot_coverage(fit1, q_upper=.9, q_lower=.1,
#                            nIters=1000, mod_name="stan",
#                            fuel_dat=fuel_data_z, prod_dat=prod_data_z,
#                            prod_start_val_vec<-meta_dat$P0,
#                            prod_m_match<-meta_dat$prod_m_match,
#                            mean_samp=4.6, sig_f_par=.8)
# (uncertainty)

