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