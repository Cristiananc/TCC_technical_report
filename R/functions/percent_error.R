percent_bias <- function(traj, rec, R, max.time){
  Ne <- traj(rec$x[rec$x <= max.time])
  N <- rec$effpop[rec$x <= max.time]
  return(100 * (1/R) * sum((abs(N - Ne))/abs(Ne)))
}