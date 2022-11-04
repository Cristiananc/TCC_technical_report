source("R/simulations_step_by_step.R")
set.seed(29)

#Setting input variables
path <- "./data/simulations/exponential/"
path_fig <- "./figs/simulations/exponential/"
n_sim <- 500
date_min <- 0
date_max <- 31

#The real trajectory of the phylogenies simulated
name_traj <- "exponential_traj1"
trajectory_exponential <- function(t, scale = 10, rate = 0.1){
  return(scale * exp(- t* rate))
}

#SIMULATION WITH 20 TAXA ####
n_taxa <- 20

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_exponential, 
                        n_taxa, name_traj, path_fig)

#SIMULATION WITH 200 TAXA ####
n_taxa <- 200
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_exponential, 
                        n_taxa, name_traj, path_fig)

#Plotting log of effective population size for both 20 and 200 taxa
summary_path <- paste0(path, name_traj)
plot_log_effepop_20_and_200_taxa (summary_path, path_fig, trajectory_exponential)

plot_pointwise_bias_for_all(summary_path, path_fig, trajectory_exponential, date_max)
