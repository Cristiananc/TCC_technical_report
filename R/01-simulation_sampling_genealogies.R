library(phylodyn)
require(rlist)
source("R/functions/01-sampling_genealogies.R")

sim_sampling_genealogies <- function(n_sim, date_min, date_max, n_taxa, trajectory, file_saving, lower_bound = 1){
  
  #Checks if the file already exists before performing the code
  if(!file.exists(file_saving)){
    plot(seq(date_min, date_max), trajectory(seq(date_min, date_max)), type = 'l') 
    
    n_sampled <- rep(1, n_taxa)
    stimes <- sort(runif(n_taxa, date_min, date_max))
    
    message("Performing the 1º step of the simulation")
    genealogies <- sampling_genealogies(n_sim = n_sim, stimes = stimes, 
                                        n_sampled = n_sampled, trajectory = trajectory,
                                        lower_bound = lower_bound)
    
    message("First step done!")
    
    list.save(genealogies, file_saving)
    message(paste("Genealogies sucessfully saved in", file_saving))
  }
  else{
    message("File already exists.")
  }
}