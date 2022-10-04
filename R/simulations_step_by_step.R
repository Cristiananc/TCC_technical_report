source("R/01-simulation_sampling_genealogies.R")
source("R/02-simulations_reconstructing_effepop.R")
source("R/03-simulations_calculating_performance_metrics.R")

simulation_step_by_step <- function(path, n_sim, date_min, date_max, traj,
                                    n_taxa, name_traj, path_fig){
  
  stimes <- sort(runif(n_taxa, date_min, date_max))
  
  #FIRST STEP #####
  #Here we simulate 500 different genealogies with 20 taxa
  outpath_01 <- paste0(path, "genealogies_", name_traj, "_", n_taxa, "taxa.rdata")
  sim_sampling_genealogies(n_sim, stimes, date_min, date_max, n_taxa, traj, outpath_01) 
  
  #SECOND STEP ###
  #The next step is the phylodynamic reconstruction of the effective population size for the
  #trees with 20 taxa for each precision prior analysed
  outpath_02 <- paste0(path, name_traj, "_", n_taxa, "taxa_")
  outpath_fig <- paste0(path_fig, name_traj, "_", n_taxa, "taxa_")
  a <- sample(1:n_sim, 1)
  sim_reconstructing_effepop(outpath_01, outpath_02, traj, n_taxa, n_sim, outpath_fig)
  
  #THIRD STEP ##########
  #Now we calculate the performance metrics
  R <- stimes[length(stimes)]
  sim_calculating_performance_metrics(traj, R, date_max, n_sim, outpath_02)
}