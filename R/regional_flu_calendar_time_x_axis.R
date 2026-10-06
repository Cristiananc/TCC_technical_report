#Analysing the different priors for real data on flu
library(phylodyn)
library(ggplot2)
library(ggpubr)
library(dplyr)

data("regional_flu")
set.seed(29)

#Parameters for the gamma prior
matching_gamma_par <- c(2.544882, 1.203437)

mains <- list(USACanada = "USA / Canada", Europe = "Europe", NorthChina = "North China",
              JapanKorea = "Japan / Korea", India = "India", SouthChina = "South China",
              SouthAmerica = "South America", SoutheastAsia = "Southeast Asia", Oceania = "Oceania")

#Base year per region
base_year <- list(USACanada = 2012.301, Europe = 2011.044, NorthChina = 2011.285,
                  JapanKorea = 2012.29, India = 2010.814, SouthChina = 2011.282,
                  SouthAmerica = 2011.518, SoutheastAsia = 2011.995, Oceania = 2010.964)

number_tips <- list(USACanada = 520, Europe = 361, NorthChina = 384,
                    JapanKorea = 444, India = 233, SouthChina = 528,
                    SouthAmerica = 191, SoutheastAsia = 494, Oceania = 461)

#Results for each region
regions <- list("USACanada", "Europe", "NorthChina", "JapanKorea", "India", "SouthChina", "SouthAmerica", "SoutheastAsia", "Oceania")
results <- list()
for (region in regions){
  results[[region]] <- list(bnpr_matching_gamma = BNPR(data = regional_flu[[region]], lengthout = 100,
                                                       prec_alpha = matching_gamma_par[1],
                                                       prec_beta = 1/matching_gamma_par[2]),
                            bnpr_pc_prior = BNPR(data = regional_flu[[region]], lengthout = 100, 
                                                 pc_prior = TRUE),
                            bnpr_gamma_flat = BNPR(data = regional_flu[[region]], lengthout =100, 
                                                   prec_alpha = 0.001, 
                                                   prec_beta = 0.001))
}

results_plot <- list()

for (region in regions){
  results_plot[[region]] <- data.frame(
    effpop975 <- c(results[[region]]$bnpr_gamma_flat$effpop975, 
                   results[[region]]$bnpr_matching_gamma$effpop975, 
                   results[[region]]$bnpr_pc_prior$effpop975),
    
    effpop025 <- c(results[[region]]$bnpr_gamma_flat$effpop025, 
                   results[[region]]$bnpr_matching_gamma$effpop025, 
                   results[[region]]$bnpr_pc_prior$effpop025), 
    
    effpop <- c(results[[region]]$bnpr_gamma_flat$effpop, 
                results[[region]]$bnpr_matching_gamma$effpop, 
                results[[region]]$bnpr_pc_prior$effpop),
    
    Prior <- rep(c("Gamma Flat", "Matching Gamma", "PC prior"), each = 100),
    
    Time <- c(results[[region]]$bnpr_gamma_flat$x, 
              results[[region]]$bnpr_matching_gamma$x, 
              results[[region]]$bnpr_pc_prior$x),
    
    effpopmean <- c(results[[region]]$bnpr_gamma_flat$effpopmean, 
                    results[[region]]$bnpr_matching_gamma$effpopmean, 
                    results[[region]]$bnpr_pc_prior$effpopmean),
    
    region_name <- rep(paste(region), 300),
    
    region_title <- rep(paste(mains[[region]], " (n = ", number_tips[[region]], ")", sep = ""), 300)
  )
}

peffpop <- rbind(results_plot[[1]], results_plot[[2]], results_plot[[3]], 
                 results_plot[[4]], results_plot[[5]], results_plot[[6]],
                 results_plot[[7]], results_plot[[8]], results_plot[[9]])

colnames(peffpop) <- c("effpop975", "effpop025", "effpop", "Prior", "Time", "effpopmean", "region_name", "region_title")

peffpop <- peffpop %>%
  mutate(Year = as.numeric(base_year[as.character(region_name)]) - Time)

options(repr.plot.width = 10, repr.plot.height = 7)
ggplot(peffpop) +
  geom_ribbon(aes(x = Year, ymin = effpop025, ymax = effpop975, fill = Prior),
              alpha = 0.1) +
  geom_line(aes(x = Year, y = effpop, col = Prior)) +
  scale_x_continuous("Time", breaks = scales::breaks_width(2)) +
  scale_y_continuous("Effective population size", expand = c(0, 0)) +
  facet_wrap(~region_title, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))
