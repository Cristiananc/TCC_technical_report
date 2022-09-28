library(ggplot2)

plot_random_reconstruction <- function(random_number, rec, traj, prior_name, col){
  plot_BNPR(rec[random_number], traj = traj,
            main= paste("BNPR: Simulation", random_number, "for", prior_name) , 
            yscale = 1,
            col = col, 
            heatmap_labels_side = "left")
}

summaries_genealogy_reconstruction <- function(n.sim, reconstructions, prior.name, ntaxa) {
  summaries_df <- do.call(rbind, lapply(1:n.sim, function(i){
    data.frame(reconstructions[[i]]$summary, replicate = i,
               prior = prior.name, ntaxa = ntaxa)}))
  return(summaries_df)
}

plot_log_effepop <- function(effepop, trajectory){
  options(repr.plot.width = 10, repr.plot.height = 7)
  peffpop <- ggplot(data = effepop) +
    scale_x_continuous("Times (years)", expand = c(0,0)) +
    scale_y_continuous("Effective population size (mean)", expand = c(0,0), trans = 'log10') +
    geom_line(aes(x = time, y = mean, col = prior, group = replicate), alpha = .5) + 
    facet_grid(ntaxa~prior, scales = "free_y") +
    guides(col = "none") +
    theme_bw() +
    stat_function(fun = trajectory, colour = 'black') +
    NULL
  
  return(peffpop)
}
