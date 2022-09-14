BCI_size <- function(traj, rec, R, max.time){
  return(100 * (1/R) * sum((rec$effpop975[rec$x <= max.time] - 
                              rec$effpop025[rec$x <= max.time])/traj(rec$x)))
}