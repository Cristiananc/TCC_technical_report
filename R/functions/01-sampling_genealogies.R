library(phylodyn)

sampling_genealogies <- 
  function(n_sim, stimes, n_sampled, trajectory, lower_bound){
    lapply(1:n_sim, function(i){
    phylodyn::coalsim(samp_times = stimes,
                    n_sampled = n_sampled,
                    traj = trajectory,
                    lower_bound = lower_bound)
    })
}