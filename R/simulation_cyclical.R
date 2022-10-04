source("R/simulations_step_by_step.R")
set.seed(29)

#Setting input variables
path <- "./data/simulations/cycllical/"
path_fig <- "./figs/simulations/cycllical/"
n_sim <- 500
date_min <- 0
date_max <- 31

#The real trajectory of the phylogenies simulated
name_traj <- "cycllical_traj1"
trajectory_cyclic <- cyclic_traj

plot(trajectory_cyclic(seq(0, 31)), type = 'l', ylab = "Effective population size", 
     xlab = "Time", xlim = c(31, 0))

#SIMULATION WITH 20 TAXA ####
n_taxa <- 20

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cycllical, 
                        n_taxa, name_traj, path_fig)

#SIMULATION WITH 200 TAXA ####
n_taxa <- 200
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cycllical, 
                        n_taxa, name_traj, path_fig)
