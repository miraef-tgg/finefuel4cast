The ‘Fuels_Model’ folder contains code that:

- Runs the Fuels Model described in Ensley-Field et al 2021
		F_(x,t) ~ Normal( f_(x,t),   〖σ^2〗_o ) ) 	   	                (Eq 1)
                f_(x,t)  ~ Normal (α*f_(x,t)+ β*P_(x,t)  ,〖σ^2〗_p) 	 		(Eq 2)
- Runs predictive posterior check version of the model
- Runs sensitivity analysis using different mean and standard deviations on all priors as well as the initial condition uncertainty term (sig_f)
- Produces trace plots and prints mean, sd, and rhat scores of parameter posterior distributions
- creates visualization of posterior predictive check (ppc) 
- creates visualization of posterior andprior distributions for each parameter
- checks coverage using parameter distributions and observed data for predictions made at t+1 
- Plots results of sensitivity analysis: plots (1) confidence intervals of resulting posterior distributions on the y axis as each parameter’s prior’s mean and standard deviation are changed one at a time on the x axis and 
- (2) changes in uncertainty resulting from each parameter as parameter’s priors are changed
- Checks model retreives parametrs from simulated data correctly
- Uses simulated data tp checks for what range of ovbservation error the model correctly retrieves an carryover(alpha) value higher than our results and it’s prior. Checks for what range of observation error the model correctly retrieves an conversion (beta) value lower than our results and it’s prior
- Shows a simulation of our thought process on starting the model’s first latent fuel load for each location from a normal distribution of the previous year’s productivify value on the standardized scale, with a standard deviation of 0.8
- Shows a linear regression of raw data, log-transformed data, deviations from long-term mean data, and standardized per location data
                 F_(x,t)  ~ α*F_(x,t-1)+ β*P_(x,t)  	 	
- All scripts used can be run in the “run_everything.R” script

inputs: (provided in 'Fuels_Model_data' folder)
fuels_model_data.rds
fuels_model_meta_data.rds


outputs:

'model_outputs' folder
-prior sensitivity analysis folder (>50 models)
-simulation folder (>50 models)
-fuels_model.rds
-fuels_model_ppcs.rds
-sim_fuels_model.rds

'Figures' folder
-ppc.png
-posterior_distributions.png

'Supplemental_Info_figs' folder:
-all traceplots (eg alpha_traceplot.png)
-simulation_retrieval.png
-spin_up_test_figs folder
-'prior_sensitivity analysis_figs' folder
-'simulation_figs' folder