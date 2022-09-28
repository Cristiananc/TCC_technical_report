require(rlist)
source("R/functions/03-calculating_performance_metrics.R")

sim_calculating_performance_metrics <- function(traj, R, max_time, n_sim, inpath){
  rec_gamma <- list.load(paste0(inpath, "reconstructions_gamma.rdata"))
  rec_matching_gamma <- list.load(paste0(inpath, "reconstructions_matching_gamma.rdata"))
  rec_pc_prior <- list.load(paste0(inpath, "reconstructions_pc_prior.rdata"))
  
  errors_gamma <- percent_error(traj, rec_gamma, R, max_time)
  errors_matching_gamma <- percent_error(traj, rec_matching_gamma, R, max_time)
  errors_pc_prior <- percent_error(traj, rec_pc_prior, R, max_time)
  
  bias_gamma <- percent_bias(traj, rec_gamma, R, max_time)
  bias_matching_gamma <- percent_bias(traj, rec_matching_gamma, R, max_time)
  bias_pc_prior <- percent_bias(traj, rec_pc_prior, R, max_time)
  
  BCI_size_gamma <- BCI_size(traj,rec_gamma, R, max_time)
  BCI_size_matching_gamma <- BCI_size(traj, rec_matching_gamma, R, max_time)
  BCI_size_pc_prior <- BCI_size(traj, rec_pc_prior, R, max_time) 
  
  metrics_df <- data.frame(
    bias = c(bias_gamma, bias_matching_gamma, bias_pc_prior),
    widths = c(BCI_size_gamma, BCI_size_matching_gamma, BCI_size_pc_prior),
    errors = c(errors_gamma, errors_matching_gamma, errors_pc_prior),
    prior = rep(c("Gamma Flat", "Matching Gamma", "PC prior"), each = n_sim)
  )
  
  #Creating a table with the Q1, Q2, Q3 and mean values
  
  #Saving the dataframes
  list.save(metrics_df, paste0(inpath, "metrics_df.rdata"))
}