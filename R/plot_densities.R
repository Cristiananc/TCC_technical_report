#Plotting densities distributions for Gamma matching, 
#PC prior and Gamma Flat for methods section

#Density plots ----------------
library(ggplot2)
library(VaRES) #Gumbel2 distribution
library(latex2exp)

#Parameters of the distributions
pc_prior <- c(0.5, 2.302585)
matching_gamma_par <- c(2.544882, 1.203437)

p <- ggplot() +
  geom_function(aes(colour = "Gamma usual"), fun = dgamma, args = list(shape = 0.001, rate = 0.001), size = 1.3) +
  geom_function(aes(colour = "PC"), fun = dgumbel2, args = list(a = pc_prior[1], b = pc_prior[2]), size = 1.3) +
  geom_function(aes(colour = "Matching Gamma"), fun = dgamma, args = list(shape = matching_gamma_par[1], scale = matching_gamma_par[2]), size = 1.3) +
  theme_bw() 

p1 <- p +
  theme(text = element_text(size=15), 
        legend.title = element_text(size=15, face = "bold"),
        axis.title.x =element_text(size= 25,face="bold"),
        axis.title.y =element_text(size= 15,face="bold")) +
  xlab(TeX("$\\tau$")) +
  ylab("Density") +
  guides(colour=guide_legend(title="Prioris")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0))

#Saving figure -----------
ggsave(path = "figs", 
       filename = "01_densities_priors_plot.png", 
       plot = p1, device = 'png')
