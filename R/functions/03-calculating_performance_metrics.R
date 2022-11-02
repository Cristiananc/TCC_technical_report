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
pointwise_bias <- function(rec_summary, n_sim, max_time, traj){
  intervals <- seq(0, max_time, length.out = 100)
  mid_point <- rollmean(intervals, 2)
  real_values <- traj(mid_point)
  
  rec_summary_bias <- rec_summary %>% 
    mutate(ranges = cut(time,
                        seq(0, max_time, length.out = 100))) %>% 
    complete(ranges) %>%
    group_by(ranges) %>%
    summarize(mean_effe_pop = mean(mean)) %>%
    mutate(bias = mean_effe_pop - c(real_values, NA)) %>%
    as.data.frame()
  
  plot(rec_summary_bias$bias)
}