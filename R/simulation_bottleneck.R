source("R/01-simulation_sampling_genealogies.R")
source("R/02-simulations_reconstructing_effepop.R")
source("R/03-simulations_calculating_performance_metrics.R")

set.seed(29)
path <- "./data/simulations/bottleneck/"

# Number of simulations
n_sim <- 5
date_min <- 0
date_max <- 31

#The real trajectory of the phylogenies simulated
trajectory_bottleneck <- function(t){
  result = rep(0, length(t))
  result[t <= 15] <- 10
  result[t > 15 & t < 31] <- 1
  result[t >= 31] <- 10
  return(result)}

#FIRST STEP #####
#Here we simulate 500 different genealogies with 20 taxa
n_taxa <- 5
stimes <- sort(runif(n_taxa, date_min, date_max))
outpath_01 <- paste0(path, "genealogies_bottleneck_traj1_", n_taxa, "taxa.rdata")
sim_sampling_genealogies(n_sim, stimes, date_min, date_max, n_taxa, trajectory_bottleneck, outpath_01) 
  
#SECOND STEP ###
#The next step is the phylodynamic reconstruction of the effective population size for the
#trees with 20 taxa for each precision prior analysed
outpath_02 <- paste0(path, "bottleneck_traj1_", n_taxa, "taxa_")
a <- sample(1:n_sim, 1)
sim_reconstructing_effepop(outpath_01, outpath_02, trajectory_bottleneck)

#THIRD STEP ##########
#Now we calculate the performance metrics
R <- stimes[length(stimes)]
sim_calculating_performance_metrics(trajectory_bottleneck, R, date_max, n_sim, outpath_02)
