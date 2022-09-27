library(phylodyn)
require(rlist)
source("R/functions/02-reconstructing_effepop.R")

sim_reconstructing_effepop <- function(genealogies_file, outpath, traj, random_number){
  
  #Loading file saved as rdata format
  genealogies <- list.load(genealogies_file)
  
  message("Performing 2º step of the simulation")
  effepop_gamma <- effepop_reconstruction_gamma(genealogies)
  effepop_matching_gamma <- effepop_reconstruction_matching_gamma(genealogies)
  effepop_pc_prior <- effepop_reconstruction_pc_prior(genealogies)
  
  list.save(effepop_gamma, paste0(outpath, "reconstructions_gamma.rdata"))
  list.save(effepop_matching_gamma, paste0(outpath, "reconstructions_matching_gamma.rdata"))
  list.save(effepop_pc_prior, paste0(outpath, "reconstructions_pc_prior.rdata"))
  
  plot_random_simulation(random_number, effepop_gamma, traj, "Gamma", "coral")
  plot_random_simulation(random_number, effepop_matching_gamma, traj, "Matching Gamma", "limegreen")
  plot_random_simulation(random_number, effepop_pc_prior_gamma, traj, "PC prior", "turquoise")
}