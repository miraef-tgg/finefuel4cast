
make_m<-function(fuel_data2){
  M<-matrix(data=NA,nrow=nrow(fuel_data2), ncol = ncol(fuel_data2))
  
  for (i in 1:nrow(fuel_data2)){
    for (j in 1:ncol(fuel_data2)){
      M[i,j]<-ifelse(is.na(fuel_data2[i,j]), NA, paste(i,",",j))
    }
  }
  
  M2<-as.vector(t(M))
  M3<-(subset(M2, !is.na(M2)))
  M4<-matrix(data=NA, nrow=length(M3),ncol=2)
  rows<-sapply(strsplit(M3, ","), "[", 1)
  cols<-sapply(strsplit(M3, ","), "[", 2)
  M<-cbind(rows,cols)
  Mrow<-as.vector(as.integer(M[,1]))
  Mcol<-as.vector(as.integer(M[,2]))
  M_final<-as.array(cbind(as.integer(Mrow),as.integer(Mcol)))
  return(M_final)
}

make_sim_data<-function(
  nPlots=148,
  nYears=25,
  sig_o=.8, #fuel obs error
  sig_p=.6, # process error
  percent_missing=5,
  show_plots=TRUE,
  random=FALSE,
  alpha=.1,
  beta=.25
){
  if(random==FALSE)(set.seed(123))
  nMiss<-percent_missing/100*(nPlots*nYears); #number of missing observations
  
  #loading productivity data
  dat<-readRDS("fuels_model_data/fuels_model_data.rds")
  prod_data_orig<-dat$prod[,1:35]
  prod<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

  for(i in 1:nrow(prod_data_orig)){
    prod[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
  }
  
  names_old<-dat$site_names
  # prod_orig<-readRDS("G:/My Drive/finefuel4cast/Fuels_model/Bayesian_Land/stan_model_scripts/stan_data/newer_data_list.Rds")
  # prod<-prod_orig$raw_prod_all_yrs_new_loc+500
  prod_init<-prod[,(ncol(prod)-nYears)]
  
  prod<-prod[,(ncol(prod)-nYears+1):ncol(prod)]
  
  row_nums<-sample(x=nrow(prod), size=nPlots) 
  plot_names<-names_old[row_nums]
  
  p_latent<-prod[row_nums,]
  prod_init<-prod[row_nums,1]
  
  
  # simulate latent fuel load data (from p_latent) #cols= yrs
  f_latent <- matrix(NA,nrow=nrow(p_latent),ncol=(ncol(p_latent)+1))
  f_latent[,1] <- abs(rnorm(nrow(f_latent),colMeans(p_latent),sig_p)) # initial year for each loc
  proc_error <- matrix(rnorm(nPlots*(nYears+1), 0, sig_p),nrow=nrow(p_latent),ncol=(ncol(p_latent)+1))
  
  for(i in 2:(ncol(f_latent))){
    f_latent[,i] <- abs(alpha*(f_latent[,i-1]) + beta*(p_latent[,i-1]) +proc_error[,i])
  }
  
  f_latent <- f_latent[,2:ncol(f_latent)] # drop initial condition year
  proc_error <- proc_error[,2:ncol(proc_error)] 
  
  #adding nSamples per plot
  sample_vec<-rep(sample(1:15, nPlots,replace=TRUE),each=(nYears))
  #mking it more realistic
  # sample_vec<-rep(sample(1:10, nPlots,replace=TRUE, prob=c(.1,.1,.3,.1,.1,rep(.3/5,5))),times=(nYears))
  
  n_samples<-matrix(sample_vec,nrow=nPlots,ncol=nYears, byrow = TRUE)
  dim(n_samples)==dim(f_latent)
  
  # observation error depends on nSamples
  f_obs<-matrix(NA,nPlots,nYears)
  obs_error<-matrix(NA,nPlots, nYears)
  for(x in 1:(nPlots)){
    for (t in 1:nYears){
      # obs_err<- mean(rnorm(n_samples[x,t],0,sig_o))
      obs_err<- mean(rnorm(1,0,sig_o))
      
      # f_obs[x,t] <- f_latent[x,t]+obs_error[x,t]
      obs_err<-rnorm(1,0,sig_o/sqrt(n_samples[x,t]))
      obs_error[x,t]<-obs_err
      f_obs[x,t] <- f_latent[x,t]+obs_err
      if(f_obs[x,t]<0) (f_obs[x,t]<-0)
    }
  }

  # missing obs data
  f_obs_all<-f_obs
  nObs<-nPlots*nYears
  na_vec<-sample(x=1:nObs, size=nMiss, replace=FALSE)
  f_obs[na_vec]<-NA
  
  M<-make_m(f_obs)
  
  # making dataframe with all params and errors, long format
  sim_df<-data.frame(matrix(ncol = 0,nrow=nYears*nPlots))
  sim_df$plot<-rep(plot_names,each=nYears)
  sim_df$year<-rep(1:nYears,nPlots)
  sim_df$f_latent<-as.vector(c(t(f_latent)))
  sim_df$f_obs<-as.vector(c(t(f_obs)))
  sim_df$f_obs_all<-as.vector(c(t(f_obs_all)))
  sim_df$prod<-as.vector(c(t(p_latent)))
  sim_df$proc_error<-as.vector(c(t(proc_error)))
  sim_df$sig_p<-sig_p
  sim_df$sig_p<-sig_p
  sim_df$obs_error<-as.vector(c(t(obs_error)))
  sim_df$sig_o<-sig_o
  sim_df$samples<-as.vector(c(t(n_samples)))
  sim_df$alpha<-alpha
  sim_df$beta<-beta
  
  #visual
  if (show_plots==TRUE) {
    ran_plot<-as.character(sim_df$plot[(rand_plot-1)*nYears+1])
    data<-subset(sim_df, sim_df$plot==ran_plot)
    min<-min(cbind(data$f_latent, data$f_obs, data$prod), na.rm=TRUE)
    max<-max(cbind(data$f_latent, data$f_obs, data$prod), na.rm=TRUE)
    
    
    plot(1, type="n", xlim=c(1,max(data$year)), ylim=c(min,max), xlab="year", ylab="fuel", main=paste0("Simulated Data: ", data$plot[1]))
    lines(x=data$year, y=data$f_latent, col="black",lwd=2)
    points(x=data$year, y=data$f_latent, col="black", pch=19)
    lines(x=data$year, y=data$f_obs, col="red")
    points(x=data$year, y=data$f_obs, col="red", pch=19)
    lines(x=data$year, y=data$prod, col="blue")
    points(x=data$year, y=data$prod, col="blue", pch=19)
    
    legend("topright",legend=c("fuel latent", "fuel observed", "Prod"),
           col=c('black', 'red','blue'), lty=1)
    
    
  }
  
  # rep(unique(sim_df$plot),each=3)
  name_data<-matrix(rep(unique(sim_df$plot),each=nYears), byrow=TRUE, nrow=nPlots)
  # field_data<-matrix(rep(unique(sim_df$field_off),each=nYears*nPlots), byrow=TRUE, nrow=nPlots)
  year_data<-matrix(rep(1:nYears,nPlots),ncol=nYears, byrow=TRUE)
  list<-list(p_latent,
             prod_init,
             (f_obs),
             year_data,
             (n_samples),
             name_data,
             M,
             sim_df,
             f_latent,
             obs_error
  )
  names(list)<-c("sim_prod", "Fstart", "sim_obs_fuel","sim_year",
                 "sim_sample", "sim_name", "M", "params" , "f_latent" , "obs_error")
  
  return(list)
}
# 
# sim_data<-make_sim_raw()
# plot(sim_data$params$f_obs, sim_data$params$prod)
# sum(sim_data$params$f_obs_all==0)
# nrow(sim_data$params)
# hist(sim_data$params$f_obs)




