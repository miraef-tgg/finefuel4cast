#---------- loading pcks, data, model-------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

fit1<-readRDS("model_outputs/fuels_model.rds")

# -------------- extracting mean and 90% CI----------------
alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
betaout<-rstan::extract(fit1, 'beta', permuted=F);betaout<-apply(betaout, 3,c)
sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)

# plots
par(mfrow=c(1,3))
png("Supplemental_Info_figs/covariance")
plot(alphaout, betaout, main=" alpha vs beta")
plot(alphaout, sig_oout, main=" alpha vs sig_o")
plot(alphaout, sig_pout, main=" alpha vs sig_p")

par(mfrow=c(1,3))
plot(betaout, alphaout, main=" beta vs alpha")
plot(betaout, sig_oout, main=" beta vs sig_o")
plot(betaout, sig_pout, main=" beta vs sig_p")

par(mfrow=c(1,3))
plot(sig_oout, alphaout, main=" sig_o vs alpha")
plot(sig_oout, betaout, main=" sig_o vs beta")
plot(sig_oout, sig_pout, main=" sig_o vs sig_p")

par(mfrow=c(1,3))
plot(sig_pout, alphaout, main=" sig_p vs alpha")
plot(sig_pout, betaout, main=" sig_p vs beta")
plot(sig_pout, sig_oout, main=" sig_p vs sig_o")


cor(alphaout, betaout)
cor(alphaout, sig_oout)
cov(alphaout, sig_pout)

