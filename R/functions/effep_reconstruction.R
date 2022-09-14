library(phylodyn)
require(rlist)

effep_reconstruction <- function(genealogies){
    
  #Gamma usual
  system.time(
    reconstructions_gamma <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, prec_alpha = 0.001, prec_beta = 0.001))
  )
  
  #Matching Gamma
  #par_ab is a vector with the parameters found for the gamma distribution we choose to work with
  par_ab <- c(2.544882, 1.203437)
  system.time(
    reconstructions_matching_gamma <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, prec_alpha = par_ab[1], prec_beta = 1/par_ab[2]))
  )
  
  #PC prior
  system.time(
    reconstructions_pc_prior <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, 
           pc_prior = TRUE)))
  
  #We export the reconstructions obtained to the data folder
  #list.save(reconstructions_gamma, "Data")
  
}