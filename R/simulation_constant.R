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