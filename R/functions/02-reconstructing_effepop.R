library(phylodyn)
library(ggplot2)
require(rlist)

effepop_reconstruction_gamma <- function(genealogies){
  #Gamma usual
  system.time(
    reconstructions_gamma <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, prec_alpha = 0.001, prec_beta = 0.001))
  )
  return(reconstructions_gamma)
}
  
effepop_reconstruction_matching_gamma <- function(genealogies){
  #Matching Gamma
  #par_ab is a vector with the parameters found for the gamma distribution we choose to work with
  par_ab <- c(2.544882, 1.203437)
  system.time(
    reconstructions_matching_gamma <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, prec_alpha = par_ab[1], prec_beta = 1/par_ab[2]))
  )
  
  return(reconstructions_matching_gamma)
}  
  
effepop_reconstruction_pc_prior <- function(genealogies){
  #PC prior
  system.time(
    reconstructions_pc_prior <- lapply(genealogies, function(x)
      BNPR(data = x, lengthout = 100, 
           pc_prior = TRUE)))
  
  return(reconstructions_pc_prior)
}
