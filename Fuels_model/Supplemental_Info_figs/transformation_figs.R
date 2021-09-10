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

par(mfrow=c(2,2))

##raw

fuel_data_prev<-as.data.frame(fuel_data_orig[,2:25])
fuel_data_prev[,25]<-NA
dat<-as.data.frame(cbind(as.vector(fuel_data_orig), as.vector(prod_data_orig), unlist(fuel_data_prev)))
names(dat) <- c("fuel", "prod", "prev")
# plot(dat$fuel, dat$prod)
plot(fuel_data_orig, prod_data_orig, main= "Raw data")
lm<-lm(fuel~prod+prev+0,data=dat)
abline(lm, col="black", lwd=2)
(alpha<-round(coef(lm)[1],2))
(beta<-round(coef(lm)[2],2))
(cor<-round(cor(dat$fuel, dat$prod,use = "complete.obs"),2))
(r2<-round(summary(lm)$r.squared,2))
mape<-round(mean(abs(lm$residuals)),2)
legend("topright", c(paste0("carryover: ",alpha), paste0("conversion: ", beta), paste0("correlation: ", cor), paste0("r squared: ", r2),
                       paste0("mean abs pred err: ", mape)), title="Predictive Scores")
  
##log
fuel_data_log<-replace(fuel_data_orig, fuel_data_orig==0,1)
prod_data_log<-replace(prod_data_orig, prod_data_orig==0,1)

fuel_data_prev<-log(as.data.frame(fuel_data_log[,2:25]))
fuel_data_prev[,25]<-NA
dat<-as.data.frame(cbind(as.vector(log(fuel_data_log)), as.vector(log(prod_data_log)), unlist(fuel_data_prev)))
names(dat) <- c("fuel", "prod", "prev")
# plot(dat$fuel, dat$prod)
plot(log(fuel_data_log), log(prod_data_orig), main= "Log-transformed data")
lm<-lm(fuel~prod+prev+0,data=dat)
abline(lm, col="black", lwd=2)
(alpha<-round(coef(lm)[1],2))
(beta<-round(coef(lm)[2],2))
(cor<-round(cor(dat$fuel, dat$prod,use = "complete.obs"),2))
(r2<-round(summary(lm)$r.squared,2))
mape<-round(mean(abs(lm$residuals)),2)
legend("topright", c(paste0("carryover: ",alpha), paste0("conversion: ", beta), paste0("correlation: ", cor), paste0("r squared: ", r2),
                     paste0("mean abs pred err: ", mape)), title="Predictive Scores")


##deviations
fuel_data_prev<-as.data.frame(fuel_data_dev[,2:25])
fuel_data_prev[,25]<-NA
dat<-as.data.frame(cbind(as.vector(fuel_data_dev), as.vector(prod_data_dev), unlist(fuel_data_prev)))
names(dat) <- c("fuel", "prod", "prev")
# plot(dat$fuel, dat$prod)
plot(fuel_data_dev, prod_data_dev, main= "Deviations data")
lm<-lm(fuel~prod+prev+0,data=dat)
abline(lm, col="black", lwd=2)
(alpha<-round(coef(lm)[1],2))
(beta<-round(coef(lm)[2],2))
(cor<-round(cor(dat$fuel, dat$prod,use = "complete.obs"),2))
(r2<-round(summary(lm)$r.squared,2))
mape<-round(mean(abs(lm$residuals)),2)
legend("topright", c(paste0("carryover: ",alpha), paste0("conversion: ", beta), paste0("correlation: ", cor), paste0("r squared: ", r2),
                     paste0("mean abs pred err: ", mape)), title="Predictive Scores")



##z

fuel_data_prev<-as.data.frame(fuel_data_z[,2:25])
fuel_data_prev[,25]<-NA
dat<-as.data.frame(cbind(as.vector(fuel_data_z), as.vector(prod_data_z), unlist(fuel_data_prev)))
names(dat) <- c("fuel", "prod", "prev")
# plot(dat$fuel, dat$prod)
plot(fuel_data_z, prod_data_z, main= "Standardized data")
lm<-lm(fuel~prod+prev+0,data=dat)
abline(lm, col="black", lwd=2)
(alpha<-round(coef(lm)[1],2))
(beta<-round(coef(lm)[2],2))
(cor<-round(cor(dat$fuel, dat$prod,use = "complete.obs"),2))
(r2<-round(summary(lm)$r.squared,2))
mape<-round(mean(abs(lm$residuals)),2)
legend("topright", c(paste0("carryover: ",alpha), paste0("conversion: ", beta), paste0("correlation: ", cor), paste0("r squared: ", r2),
                     paste0("mean abs pred err: ", mape)), title="Predictive Scores")
