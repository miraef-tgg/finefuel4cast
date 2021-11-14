## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(corrplot,dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df_orig<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
model_df<-model_df_orig

#mod1: linear forecast
mod1_data<-subset(model_df, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                 prev_pfg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm))

                          
#look at correlation amongst covariates
cor_data<-subset(mod1_data,select=c(z_agb, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac))

cor_m<-cor(cor_data)
plot.new()
par(mfrow=c(1,1))

png("figures/covariate_correlation.png")
corrplot(cor_m, main="Correlation of weather covariates")
dev.off()

lm<-lm(z_agb~prev_z_agb+ z_pr+z_tmmx+z_vpd+z_ndvi+pr_frac
     +prev_pfg_frac+z_ssm+z_bulk_dens+z_pr_z_ssm, data=mod1_data)

coefficients(lm)
#coeffs
names_coefs<-c("int",  names(mod1_data))
names_coefs<-names_coefs[!(names_coefs=="z_agb" | names_coefs=="yr" )]
lm_coeffs<-as.data.frame(cbind(names_coefs,as.vector(coef(lm)) ))
# View(lm_coeffs)



write.csv(lm_coeffs,paste0("prod_model_outputs/lm_coeffs.csv"), row.names = FALSE)

