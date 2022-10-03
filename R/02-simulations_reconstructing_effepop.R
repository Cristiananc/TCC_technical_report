library(phylodyn)
require(rlist)
source("R/functions/02-reconstructing_effepop.R")
source("R/functions/02.1-plotting_effepop.R")

sim_reconstructing_effepop <- function(genealogies_file, outpath, traj, ntaxa, nsim, outpath_fig){
  
  #Loading file saved as rdata format
  genealogies <- list.load(genealogies_file)
  
  message("Performing 2º step of the simulation")
  file_name_gamma <-paste0(outpath, "reconstructions_gamma")
  file_name_matching_gamma <-paste0(outpath, "reconstructions_matching_gamma")
  file_name_pc_prior <- paste0(outpath, "reconstructions_pc_prior")
  ext <- ".rdata"
  
  if(!file.exists(paste0(file_name_gamma, ext))){
    system.time(effepop_gamma <- effepop_reconstruction_gamma(genealogies))
    sgamma <- summaries_genealogy_reconstruction(nsim, effepop_gamma, "Gamma", ntaxa)
    
    list.save(sgamma, paste0(file_name_gamma, "_summary", ext))
    list.save(effepop_gamma, paste0(file_name_gamma, ext))
    message(paste("Reconstruction successfully saved in"), file_name_gamma)
  }
  else{
    message("Reconstruction for Gamma file already exist.")
    effepop_gamma <-list.load(paste0(outpath, "reconstructions_gamma.rdata"))
  }
  
  if(!file.exists(paste0(file_name_matching_gamma, ext))){
    system.time(effepop_matching_gamma <- effepop_reconstruction_matching_gamma(genealogies))
    smgamma <- summaries_genealogy_reconstruction(nsim, effepop_matching_gamma, "Matching Gamma", ntaxa)
    
    list.save(smgamma, paste0(file_name_matching_gamma, "_summary" , ext))
    list.save(effepop_matching_gamma, paste0(file_name_matching_gamma, ext))
    message(paste("Reconstruction successfully saved in"), file_name_matching_gamma)
  }
  else{
    message("Reconstruction for Matching Gamma file already exist.")
    effepop_matching_gamma <-list.load(paste0(outpath, "reconstructions_matching_gamma.rdata"))
  }
  
  if(!file.exists(paste0(file_name_pc_prior, ext))){
    system.time(effepop_pc_prior <- effepop_reconstruction_pc_prior(genealogies))
    spcprior <- summaries_genealogy_reconstruction(nsim, effepop_pc_prior, "PC Prior", ntaxa)
    
    list.save(spcprior, paste0(file_name_pc_prior, "_summary" , ext))
    list.save(effepop_pc_prior, paste0(file_name_pc_prior, ext))
    message(paste("Reconstruction successfully saved in"), file_name_pc_prior)
  }
  else{
    message("Reconstruction for PC Prior file already exists.")
    effepop_pc_prior <- list.load(paste0(outpath, "reconstructions_pc_prior.rdata"))
  }
  
  message("Second step done!")
  
  #Ploting one random reconstruction for each prior distribution
  message("Plotting one random reconstruction for each precision prior distribution.")
  rn <- sample(1:nsim, 1)
  par(mfrow=c(3,1))
  plot_random_reconstruction(effepop_gamma[[rn]], traj, "Gamma", "coral", rn)
  plot_random_reconstruction(effepop_matching_gamma[[rn]], 
                             traj, "Matching Gamma", "limegreen", rn)
  plot_random_reconstruction(effepop_pc_prior[[rn]], traj, 
                             "PC Prior", "turquoise", rn)
  
  dev.copy2pdf(file = paste0(outpath_fig, "plot_reconstruction.pdf"), out.type = "pdf")
  par(mfrow = c(1, 1))
}
