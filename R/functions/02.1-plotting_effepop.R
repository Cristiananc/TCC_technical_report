library(ggplot2)

plot_random_reconstruction <- function(rec, traj, prior_name, col, random_number){
  plot_BNPR(rec, traj = traj,
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
    scale_y_continuous("Log of Effective population size (mean)", expand = c(0,0), trans = 'log10') +
    geom_line(aes(x = time, y = mean, col = prior, group = replicate), alpha = .5) + 
    facet_grid(ntaxa~prior, scales = "free_y") +
    guides(col = "none") +
    theme_bw() +
    stat_function(fun = trajectory, colour = 'black')
  
  show(peffpop)
}

plot_log_effepop_20_and_200_taxa <- function(summary_path, path_fig, traj){
  file_ext <- "_summary.rdata"
  sgamma20 <- list.load(paste0(summary_path, "_20taxa_reconstructions_gamma", file_ext))
  s_mgamma20 <- list.load(paste0(summary_path,"_20taxa_reconstructions_matching_gamma", file_ext))
  spcprior20 <- list.load(paste0(summary_path, "_20taxa_reconstructions_pc_prior", file_ext))
  
  sgamma200 <- list.load(paste0(summary_path, "_200taxa_reconstructions_gamma", file_ext))
  s_mgamma200 <- list.load(paste0(summary_path,"_200taxa_reconstructions_matching_gamma", file_ext))
  spcprior200 <- list.load(paste0(summary_path, "_200taxa_reconstructions_pc_prior", file_ext))
  
  all_estimates <- rbind(sgamma20, sgamma200, s_mgamma20, s_mgamma200, spcprior20, spcprior200)
  all_estimates$ntaxa <- as.factor(all_estimates$ntaxa)
  all_estimates$replicate <- as.factor(all_estimates$replicate)
  
  plot_log_effepop(all_estimates, traj)
  dev.copy2pdf(file = paste0(path_fig, "plot_log_effepop_size.pdf"), 
             width = 12, height = 8, out.type = "pdf")
}