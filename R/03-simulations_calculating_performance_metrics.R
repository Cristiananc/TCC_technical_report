require(rlist)
library(dplyr)
source("R/functions/03-calculating_performance_metrics.R")

sim_calculating_performance_metrics <- function(traj, R, max_time, n_sim, inpath){
  if(!file.exists(paste0(inpath, "metrics_df.rdata"))){
    rec_gamma <- list.load(paste0(inpath, "reconstructions_gamma.rdata"))
    rec_matching_gamma <- list.load(paste0(inpath, "reconstructions_matching_gamma.rdata"))
    rec_pc_prior <- list.load(paste0(inpath, "reconstructions_pc_prior.rdata"))
    
    errors_gamma <- unlist(lapply(rec_gamma, percent_error, traj = traj, R = R, max_time = max_time))
    errors_matching_gamma <- unlist(lapply(rec_matching_gamma, percent_error, 
                                           traj = traj, R = R, max_time = max_time))
    errors_pc_prior <- unlist(lapply(rec_pc_prior, percent_error, 
                                     traj = traj, R = R, max_time = max_time))
    
    bias_gamma <- unlist(lapply(rec_gamma, percent_bias, 
                                traj = traj, R = R, max_time = max_time))
    bias_matching_gamma <- unlist(lapply(rec_matching_gamma, percent_bias, 
                                         traj = traj, R = R, max_time = max_time))
    bias_pc_prior <- unlist(lapply(rec_pc_prior, percent_bias, 
                                   traj = traj, R = R, max_time = max_time))
    
    BCI_size_gamma <- unlist(lapply(rec_gamma, BCI_size, traj = traj, R = R, max_time = max_time))
    BCI_size_matching_gamma <- unlist(lapply(rec_matching_gamma, BCI_size, 
                                             traj = traj, R = R, max_time = max_time))
    BCI_size_pc_prior <- unlist(lapply(rec_pc_prior, BCI_size, 
                                       traj = traj, R = R, max_time = max_time))
    
    metrics_df <- data.frame(
      bias = c(bias_gamma, bias_matching_gamma, bias_pc_prior),
      widths = c(BCI_size_gamma, BCI_size_matching_gamma, BCI_size_pc_prior),
      errors = c(errors_gamma, errors_matching_gamma, errors_pc_prior),
      prior = rep(c("Gamma Flat", "Matching Gamma", "PC prior"), each = n_sim)
    )
    
    list.save(metrics_df, paste0(inpath, "metrics_df.rdata"))  
  }
  else{
    message("File with performance metrics results already exists!")
    metrics_df <- list.load(paste0(inpath, "metrics_df.rdata"))
  }  
  
  #Calculating the Q1, Q2, Q3 quantiles and mean values for performance metrics 
  #of all prior distributions
  multiple.func <- function(x){
    c(quantile(x, probs = c(0.25, 0.5, 0.75)), mean(x))
  }
  
  for(i in c("PC prior", "Matching Gamma", "Gamma Flat")){
    table_stats <- do.call("rbind", lapply(metrics_df[metrics_df$prior == i, 1:3], multiple.func))
    colnames(table_stats) <- c("25%", "50%", "75%", "Mean")
    message(paste(i,"perfomance metrics:"))
    print(table_stats) 
  }
}