# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path
BL_folder<-dirname(dirname((current_path)))

(current_folder <- (dirname(getActiveDocumentContext()$path)))
setwd(current_folder)

# ----------------------- run models ----------------------

# run models
source(file = "fuels_model_run/fuels_model_run.R")
source(file = "fuels_model_run/fuels_model_run_ppc.R")
# source(file = "fuels_model_run/fuels_model_prior_sensitivity_analysis.R") #takes a long time (days) to run

#model code
file.show("stan_txts/fuels_model_code.txt")

#-------model checking and visualization ----------- 
#our favorite way to check a model
fit1<-readRDS( "model_outputs/fuels_model.rds")
shinystan::launch_shinystan(fit1)

#traceplots and rhat scores
source(file= "model_checking_and_validation/convergeance_check.R")

#ppc
source(file= "model_checking_and_validation/predictive_posterior_plots.R")

#covariance between parameters
source(file= "model_checking_and_validation/covariance_vis.R")

#prior posterior plots
source(file= "model_checking_and_validation/prior_posterior_plots.R")

# coverage:
source(file="model_checking_and_validation/coverage_function.R")
uncertainty<-plot_coverage(fit1, q_upper=.975, q_lower=.025,
                           nIters=1000, mod_name="(Default model)",
                           fuel_dat=fuel_data_z, prod_dat=prod_data_z,
                           prod_start_val_vec<-meta_dat$P0,
                           prod_m_match<-meta_dat$prod_m_match,
                           mean_samp=4.6, sig_f_par=.8, show_plot=T)


#sensitivity analyses vis: this will take a long time (>30 min) to run
# you can look at diagnostic plots, coverage plots, and changes in posteriors in "Supplemental_Info_figs/prior_sensitivity_analysis_figs" 
# all plots are organized such that x axis is changes in one part of a prior
source(file="model_checking_and_validation/sensitivity_analysis_vis.R")



# simulation
# simulates productivity data from actual productivity data
# simulates latent fuels data from fuels model
#looks at how sensitive latent fuel estimation is to initial value (not very)
source(file="Supplemental_Info_figs/initial_conditions_spin_up.R")

# a quick look using linear regression at how parameter estimates of carryover (a), conversion (b),
# as well as correlation, r squared, and mean absolute predictive error change by data transformation
source(file="Supplemental_Info_figs/transformation_figs.R")


# Supplemental information:
