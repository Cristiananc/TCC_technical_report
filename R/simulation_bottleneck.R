library(phylodyn)
require(rlist)
source("R/functions/sampling_genealogies.R")

# Number of simulations
n.sim <- 5
date_min <- 0
date_max <- 31

set.seed(29)

#The real trajectory of the phylogenies simulated
trajectory_bottleneck <- function(t){
  result = rep(0, length(t))
  result[t <= 15] <- 10
  result[t > 15 & t < 31] <- 1
  result[t >= 31] <- 10
  return(result)
}

plot(trajectory_bottleneck(seq(0, 31)), type = 'l') 

#Here we simulate 500 different genealogies with 20 taxa
n.taxa <- 5
stimes <- sort(runif(n.taxa, date_min, date_max))

genealogies <- sampling_genealogies(n.sim = n.sim, stimes = stimes, 
                     n_sampled = rep(1, n.taxa), trajectory = trajectory_bottleneck,
                     lower_bound = 1)

tree <- sample_tree(genealogies[[1]])
plot(tree)

#The next step is the phylodynamic reconstruction of the effective population size for the
#trees with 20 taxa for each precision prior analysed


