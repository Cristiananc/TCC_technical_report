#The reconstruction of the effective population size can go beyond the max.time
#we use as parameter in the simulation, but when carrying out performance metrics, 
#we follow (HALL; WOOLHOUSE; RAMBAUT, 2016) and ignore
#the values outside of the interval used for the sampling times.

percent_bias <- function(traj, rec, R, max.time){
  Ne <- traj(rec$x[rec$x <= max.time])
  N <- rec$effpop[rec$x <= max.time]
  return(100 * (1/R) * sum((N - Ne)/Ne))
}
