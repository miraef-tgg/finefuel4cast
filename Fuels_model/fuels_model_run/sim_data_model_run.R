#This runs the final version of the Fuels Model and saves the output

# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

source(file = "model_checking_and_validation/make_sim_data_function.R")

# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds")

sim_dat<-make_sim_data(  nPlots=198,
                         nYears=25,
                         sig_o=.8, #fuel obs error
                         sig_p=.7, # process error
                         percent_missing=0,
                         show_plots=TRUE,
                         random=FALSE,
                         alpha=.1,
                         beta=.25)

meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

row_keeps<-meta_dat$row_keeps
fuel_data_z<-as.data.frame(sim_dat$sim_obs_fuel[row_keeps,])
prod_data_z<-as.data.frame(sim_dat$sim_prod[row_keeps,])

#make 'prod_data_vec' from 
#replace prod indexed over with NAs matching observed data; then remove NA's
prod_incl<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))
prod_replace<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))

for ( i in 1:nrow(prod_data_z) ){
  replacement<-as.vector(prod_data_z[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))])
  prod_incl[i,(1:(year_length[i,1]))]<-unlist(replacement[(1:(year_length[i,1]))])
  prod_replace[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))]<-unlist(replacement)
}


prod_m<-make_m(prod_replace)
nOutputs<-nrow(prod_m)

#is it a first value?
first_val_vec<-vector()
for ( i in 2:nrow(prod_m)){
  prev<-i-1
  if (prod_m[i,1]!=prod_m[prev,1]) (first_val_vec[i]<-1)
  if (prod_m[i,1]==prod_m[prev,1]) (first_val_vec[i]<-0)
  
}

first_val_vec[1]<-1
prod_data_vec<-as.numeric(na.omit(as.vector(t(as.matrix(prod_replace)))))

P0<-(sim_dat$Fstart[row_keeps])
sample_data<-as.data.frame(sim_dat$sim_samp[row_keeps])
year_length<-as.data.frame(meta_dat$year_length)

# View(as.data.frame(prod_data_vec))
# ----------------------- stan code ----------------------
code<-readtext("stan_txts/fuels_model_code.txt")$text


stan.data.sim<-list("nLocs"=meta_dat$nLocs, "nYears"=meta_dat$nYears, "nMiss"=meta_dat$nMiss, "nOutputs"=meta_dat$nOutputs,
                    "P0"=as.vector(P0),
                    "P"= as.numeric(as.vector(prod_data_vec)),"first_val_vec"=as.data.frame(first_val_vec), "S"=sample_data,
                    "fuel_obs_index"=as.data.frame(meta_dat$fuels_obs_index), "prod_index"=as.data.frame(meta_dat$prod_index), "O"=fuel_data_z)



fit1<-stan(model_code=code,data=stan.data.sim,control=list(adapt_delta=.8, max_treedepth=10),warmup=20000,iter=24000, chains=4, thin=4)

saveRDS(fit1, "model_outputs/sim_fuels_model.rds")
print(paste0("divergent iterations: " , sum(get_divergent_iterations(fit1))))
print(summary(fit1)[[1]][seq(1,5,1),c(1,10)])


