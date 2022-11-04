library(dplyr)
library(zoo)
library(tidyr)

#The reconstruction of the effective population size can go beyond the max_time
#we use as parameter in the simulation, but when carrying out performance metrics, 
#we follow (HALL; WOOLHOUSE; RAMBAUT, 2016) and ignore
#the values outside of the interval used for the sampling times.

percent_bias <- function(traj, rec, R, max_time){
  Ne <- traj(rec$x[rec$x <= max_time])
  N <- rec$effpop[rec$x <= max_time]
  return(100 * (1/R) * sum((N - Ne)/Ne))
}

percent_error <- function(traj, rec, R, max_time){
  Ne <- traj(rec$x[rec$x <= max_time])
  N <- rec$effpop[rec$x <= max_time]
  return(100 * (1/R) * sum((abs(N - Ne))/abs(Ne)))
}

BCI_size <- function(traj, rec, R, max_time){
  return( (1/R) * sum((rec$effpop975[rec$x <= max_time] - 
                         rec$effpop025[rec$x <= max_time])/traj(rec$x)))
}

#Point wise bias
pointwise_bias <- function(rec_summary, max_time, traj, prior_name, n_taxa){
  intervals <- seq(0, max_time, length.out = 100)
  mid_point <- rollmean(intervals, 2)
  real_values <- traj(mid_point)
  midpoint_aux <- c(mid_point, 31)
  
  rec_summary_bias <- rec_summary %>% 
    mutate(ranges = cut(time,
                        seq(0, max_time, length.out = 100))) %>% 
    complete(ranges) %>%
    group_by(ranges) %>%
    summarize(mean_effe_pop = mean(mean)) %>%
    mutate(bias = mean_effe_pop - c(real_values, NA)) %>%
    mutate(prior = rep(prior_name, 100)) %>%
    mutate(midpoint = midpoint_aux) %>%
    #mutate(prior = rep(, 100)) %>%
    as.data.frame()
  
  return(rec_summary_bias)
}

plot_pointwise_bias_for_all <- function(summary_path, path_fig, traj, max_time){
  file_ext <- "_summary.rdata"
  sgamma20 <- list.load(paste0(summary_path, "_20taxa_reconstructions_gamma", file_ext))
  s_mgamma20 <- list.load(paste0(summary_path,"_20taxa_reconstructions_matching_gamma", file_ext))
  spcprior20 <- list.load(paste0(summary_path, "_20taxa_reconstructions_pc_prior", file_ext))
  
  sgamma200 <- list.load(paste0(summary_path, "_200taxa_reconstructions_gamma", file_ext))
  s_mgamma200 <- list.load(paste0(summary_path,"_200taxa_reconstructions_matching_gamma", file_ext))
  spcprior200 <- list.load(paste0(summary_path, "_200taxa_reconstructions_pc_prior", file_ext))
  
  sgamma20b <- pointwise_bias(sgamma20, max_time, traj, "gamma")
  s_mgamma20b <- pointwise_bias(s_mgamma20, max_time, traj, "matching gamma")
  spcprior20b <- pointwise_bias(spcprior20, max_time,traj, "pc prior")
  
  #sgamma200b <- pointwise_bias(sgamma20, max_time, traj, "gamma")
  #s_mgamma200b <- pointwise_bias(s_mgamma20, max_time, traj, "matching gamma")
  #spcprior200b <- pointwise_bias(spcprior20, max_time, traj, "pc prior")
  
  all_estimates <- rbind(sgamma20b, s_mgamma20b, spcprior20b) #, s_mgamma200b, spcprior20b, spcprior200b)
  
  print(all_estimates)
  plot_point_wise_bias_all <- ggplot(data = all_estimates) + 
    geom_line(aes(x = midpoint, y = bias, group = prior, color = prior), alpha = .5) + 
    theme_bw()
  
  plot_point_wise_bias_all
}