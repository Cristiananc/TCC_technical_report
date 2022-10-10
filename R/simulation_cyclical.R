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
#"cyclical_traj1_4taxa_reconstructions_pc_prior_summary"
sgamma20 <- list.load()
sgamma200 <- list.load()

s_mgamma20 <- list.load()
s_mgamma200 <- list.load()

spcprior20 <- list.load()
spcprior200 <- list.load()

all_estimates <- rbind(sgamma20, sgamma200, s_mgamma20, s_mgamma200, spcprior20, spcprior200)
all_estimates$ntaxa <- as.factor(all_estimates$ntaxa)
all_estimates$replicate <- as.factor(all_estimates$replicate)

plot_log_effepop(all_estimates, trajectory_cyclic)
