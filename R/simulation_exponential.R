library(phylodyn)
require(rlist)
source("R/01-simulation_sampling_genealogies.R")
source("R/02-simulations_reconstructing_effepop.R")
#source("R")

set.seed(29)

# Number of simulations
n_sim <- 5
date_min <- 0
date_max <- 31
#inpath = "./data/simulations/"
#outpath = "./data/processed/"

#The real trajectory of the phylogenies simulated
trajectory_exponential <- function(t, scale = 10, rate = 0.1){
  return(scale * exp(- t* rate))
}

#Here we simulate 500 different genealogies with 20 taxa
n_taxa <- 5
#file_name <- paste0(inpath, "")

sim_sampling_genealogies(n_sim, date_min, date_max, n_taxa, trajectory_bottleneck) 

path <- "./data/simulations/"

#The next step is the phylodynamic reconstruction of the effective population size for the
#trees with 20 taxa for each precision prior analysed
