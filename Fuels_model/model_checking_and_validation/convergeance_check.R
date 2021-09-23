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

fit1<-readRDS( "model_outputs/fuels_model_ppc.Rds")

png( "Supplemental_Info_figs/alpha_traceplot.png")
traceplot(fit1, "alpha")
dev.off()

png( "Supplemental_Info_figs/beta_traceplot.png")
traceplot(fit1, "beta")
dev.off()

png( "Supplemental_Info_figs/sig_o_traceplot.png")
traceplot(fit1, "sig_o")
dev.off()

png( "Supplemental_Info_figs/sig_f_traceplot.png")
traceplot(fit1, "sig_f")
dev.off()


#---------------------------------- check for retrieval, mixing, etc
print(summary(fit1)[[1]][seq(1,4,1),c(1,3,10)])

