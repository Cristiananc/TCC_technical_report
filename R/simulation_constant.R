source("R/simulations_step_by_step.R")
set.seed(29)

#Setting input variables
path <- "./data/simulations/constant/"
path_fig <- "./figs/simulations/constant/"
n_sim <- 500
date_min <- 0
date_max <- 31

#The real trajectory of the phylogenies simulated
name_traj <- "constant_traj1"
trajectory_constant <- function(x){
  return(rep(10, length(x)))
}

# SIMULATION WITH 20 TAXA ####
n_taxa <- 20

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_constant, 
                        n_taxa, name_traj, path_fig)

# SIMULATION WITH 200 TAXA ####
n_taxa <- 200
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_constant, 
                        n_taxa, name_traj, path_fig)

#Plotting log of effective population size for both 20 and 200 taxa
summary_path <- paste0(path, name_traj, "_500n_sim")
plot_log_effepop_20_and_200_taxa (summary_path, path_fig, trajectory_constant)

#Pointwise bias
plot_pointwise_bias_for_all(summary_path, path_fig, trajectory_constant, date_max)
