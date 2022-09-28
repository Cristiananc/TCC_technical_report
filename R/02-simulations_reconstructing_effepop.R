library(phylodyn)
require(rlist)
source("R/functions/02-reconstructing_effepop.R")

sim_reconstructing_effepop <- function(genealogies_file, outpath, traj){
  
  #Loading file saved as rdata format
  genealogies <- list.load(genealogies_file)
  
  message("Performing 2º step of the simulation")
  file_name_gamma <-paste0(outpath, "reconstructions_gamma.rdata")
  file_name_matching_gamma <-paste0(outpath, "reconstructions_matching_gamma.rdata")
  file_name_pc_prior <- paste0(outpath, "reconstructions_pc_prior.rdata")
  
  if(!file.exists(file_name_gamma)){
    effepop_gamma <- effepop_reconstruction_gamma(genealogies)
    list.save(effepop_gamma, file_name_gamma)
    message(paste("Reconstruction successfully saved in"), file_name_gamma)
  }
  else if(!file.exists(file_name_matching_gamma)){
    effepop_matching_gamma <- effepop_reconstruction_matching_gamma(genealogies)
    list.save(effepop_gamma, file_name_matching_gamma)
    message(paste("Reconstruction successfully saved in"), file_name_matching_gamma)
  }
  
  else if(!file.exists(file_name_pc_prior)){
    effepop_pc_prior <- effepop_reconstruction_pc_prior(genealogies)
    list.save(effepop_pc_prior, file_name_pc_prior)
    message(paste("Reconstruction successfully saved in"), file_name_pc_prior)
  }
  else{
    message("All files already exist.")
  }
}