source("R/simulations_step_by_step.R")

set.seed(29)

# Setting input variables
path <- "./data/simulations/bottleneck/"
path_fig <- "./figs/simulations/bottleneck"
n_sim <- 5
date_min <- 0
date_max <- 31
n_taxa <- 5
name_traj <- "bottleneck_traj1"

#The real trajectory of the phylogenies simulated
trajectory_bottleneck <- function(t){
  result = rep(0, length(t))
  result[t <= 15] <- 10
  result[t > 15 & t < 31] <- 1
  result[t >= 31] <- 10
  return(result)}

#Performing simulations:
simulation_step_by_step(path, n_sim, date_min, date_max, trajectory_bottleneck, 
                        n_taxa, name_traj, path_fig)
 