# ----------------------- packges, fncs----------------------
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

fit1<-readRDS( "model_outputs/fuels_model_ppc.rds")

# ------------------------- posterior predictive check -----------------

#extracting frep, F,o_rep,
F_rep<-rstan::extract(fit1, 'Fvec_rep', permuted=F); F_rep<-apply(F_rep, 3,c)
Fvec<-rstan::extract(fit1,'Fvec', permuted=F); Fvec<-apply(Fvec,3,c)
sig_o<-rstan::extract(fit1,'sig_o', permuted=F); sig_o<-apply(sig_o,3,c)
o_rep<-rstan::extract(fit1,'O_rep', permuted=F);o_rep<-apply(o_rep,3,c)
o_data<-rstan::extract(fit1,'O_data_out', permuted=F);o_data<-apply(o_data,3,c)

dim(o_rep) #25*148 cols, #1100 not NAs
dim(F_rep)

#need to subset o_rep and o_data to matching 1165; using prod_m_match and indexing
nLocs<-length(unique(meta_dat$fuels_obs_index[,1]))
nyears<-length(unique(meta_dat$fuels_obs_index[,2]))
o_rep_locs<-rep(seq(1,nLocs), nyears)
o_rep_yrs<-rep(seq(1,nyears), each=nLocs)
o_rep_loc_yrs<-paste0(o_rep_locs, "_", o_rep_yrs)

col_nums<-as.data.frame(cbind(o_rep_loc_yrs,seq(1,ncol(o_rep))))
names(col_nums)<-c("loc_yr", "index")

for ( i in 1:nrow(col_nums)){
  col_nums$match[i]<- (col_nums$loc_yr[i] %in% meta_dat$prod_m_match$loc_yr)
}

table(col_nums$match) #1165
col_nums<-col_nums[col_nums$match,]

o_rep<-o_rep[,as.numeric(col_nums$index)]
o_data<-o_data[,as.numeric(col_nums$index)]
dim(o_rep) ==dim(Fvec)

nIters<-nrow(o_rep)
loglikrep<-matrix(0,1,nIters)
loglikdata<-matrix(0,1,nIters)
likrep<-matrix(0,ncol(o_rep),nIters)
likdata<-matrix(0,ncol(o_rep),nIters)

log_sum_count=0
log_count=0

for (iter in 1:nIters){ #n
  for (i in 1:1165){ #loc/yr
    likrep[i,iter]<-(dnorm(o_rep[iter, i],F_rep[iter,i],sig_o[iter]/sample_data[i,1], log=T))
    likdata[i,iter]<-(dnorm(o_data[iter,i],Fvec[iter,i],sig_o[iter]/sample_data[i,1], log=T))
  }
  loglikrep[,iter]<-sum(likrep[,iter],na.rm = TRUE)
  loglikdata[,iter]<-sum(likdata[,iter],na.rm=TRUE)
  if(loglikrep[,iter]>loglikdata[,iter]){log_sum_count=log_sum_count+1}
}

log_sum_count/(nrow(loglikdata)*ncol(loglikdata))


#save
png("Figures/ppc.png")
par(mfrow=c(1,1))
plot(-2*loglikdata,-2*loglikrep, ylab="T(Replicated Data|Theta)",xlab="T(Data|Theta)", main="standardized model: log likelihood")
abline(0,1,col="red")
dev.off()

#look
plot(-2*loglikdata,-2*loglikrep, ylab="T(Replicated Data|Theta)",xlab="T(Data|Theta)", main="standardized model: log likelihood")
abline(0,1,col="red")

