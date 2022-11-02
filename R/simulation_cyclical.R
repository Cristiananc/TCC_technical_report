source("R/simulations_step_by_step.R")
set.seed(29)

#Setting input variables
path <- "./data/simulations/cyclical/"
path_fig <- "./figs/simulations/cyclical/"
n_sim <- 500
date_min <- 0
date_max <- 31  

#The real trajectory of the phylogenies simulated
name_traj <- "cyclical_traj1"
trajectory_cyclic <- cyclic_traj

plot(trajectory_cyclic(seq(0, 31)), type = 'l', ylab = "Effective population size", 
     xlab = "Time", xlim = c(31, 0))

#SIMULATION WITH 20 TAXA ####
n_taxa <- 20

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cyclic, 
                        n_taxa, name_traj, path_fig)

#SIMULATION WITH 200 TAXA ####
n_taxa <- 200
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cyclic, 
                        n_taxa, name_traj, path_fig)

#Plotting log of effective population size for both 20 and 200 taxa
summary_path <- paste0(path, name_traj)
plot_log_effepop_20_and_200_taxa (summary_path, path_fig, trajectory_cyclic)

#Testing with another trajectory, 20 taxa but now only 50 simulations
n_sim <- 50
trajectory_cyclic <- cyclic_traj

plot(trajectory_cyclic(seq(0, 31)), type = 'l', ylab = "Effective population size", 
     xlab = "Time", xlim = c(31, 0))

cyclic_traj = function(t)
{
  result = rep(0, length(t))
  result[(t %% 20) <= 10] = 200*exp(-(t[(t %% 20) <= 10] %% 20) / 2)
  result[(t %% 20) >  10] = 200*exp((t[(t %% 20) >  10] %% 10) / 2 - 5)
  return(result)
}

n_taxa <- 20

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cyclic, 
                        n_taxa, name_traj, path_fig)

n_taxa <- 200

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_cyclic, 
                        n_taxa, name_traj, path_fig)

#Plotting log of effective population size for both 20 and 200 taxa
summary_path <- paste0(path, name_traj, "_50n_sim")
plot_log_effepop_20_and_200_taxa (summary_path, path_fig, trajectory_cyclic)
