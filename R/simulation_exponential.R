library(ggplot2)
library(phylodyn)
require(rlist)

set.seed(29)

date_min <- 0
date_max <- 31

trajectory_exponential <- function(t, scale = 10, rate = 0.1){
  return(scale * exp(- t* rate))
}

#Plotting the trajectory
plot(trajectory_exponential(seq(0, 31)), type = 'l')

n.sim <- 500 # number of simulations

#20 taxa
stimes20 <- sort(runif(20, date_min, date_max))

genealogies_20taxa <- lapply(1:n.sim, function(i){
  phylodyn::coalsim(samp_times = stimes20,
                    n_sampled = rep(1,20),
                    traj = trajectory_exponential,
                    lower_bound = 0)
})

#Ploting one of the trees
tree <- sample_tree(genealogies_20taxa[[1]])
plot(tree)

#200 taxa
stimes200 <- sort(runif(200, date_min, date_max))

genealogies_200taxa <- lapply(1:n.sim, function(i){
  phylodyn::coalsim(samp_times = stimes200,
                    n_sampled = rep(1,200),
                    traj = trajectory_exponential,
                    lower_bound = 0)
})

#Ploting one of the trees
tree200 <- sample_tree(genealogies_200taxa[[1]])
plot(tree200)

#Gamma Flat
system.time(
  reconstructions_20taxa_gamma <- lapply(genealogies_20taxa, function(x)
    BNPR(data = x, lengthout =100, prec_alpha = 0.001, prec_beta = 0.001))
)

#PC Prior
system.time(
  reconstructions_20taxa_gumbel <- lapply(genealogies_20taxa, function(x)
    BNPR(data = x, lengthout = 100, 
         pc_prior = TRUE))
)

#Matching Gamma
par_ab <- c(2.544882, 1.203437)
system.time(
  reconstructions_20taxa_gamma2 <- lapply(genealogies_20taxa, function(x)
    BNPR(data = x, lengthout =100, prec_alpha = par_ab[1], prec_beta = 1/par_ab[2]))
)

#Phylodynamic reconstruction of effective population size  200 taxa

#Gamma Flat
system.time(
  reconstructions_200taxa_gamma <- lapply(genealogies_200taxa, function(x)
    BNPR(data = x, lengthout = 100, prec_alpha = 0.001, prec_beta = 0.001))
)

#PC prior
system.time(
  reconstructions_200taxa_gumbel <- lapply(genealogies_200taxa, function(x)
    BNPR(data = x, lengthout = 100, 
         pc_prior = TRUE))
)

#Matching Gamma
par_ab <- c(2.544882, 1.203437)
system.time(
  reconstructions_200taxa_gamma2 <- lapply(genealogies_200taxa, function(x)
    BNPR(data = x, lengthout =100, prec_alpha = par_ab[1], prec_beta = 1/par_ab[2]))
)

all_estimates <- rbind(summaries_gamma_20taxa, summaries_gamma2_20taxa,
                       summaries_gumbel_20taxa,
                       summaries_gamma_200taxa,
                       summaries_gamma2_200taxa, summaries_gumbel_200taxa)

all_estimates$ntaxa <- as.factor(all_estimates$ntaxa)
head(all_estimates)

