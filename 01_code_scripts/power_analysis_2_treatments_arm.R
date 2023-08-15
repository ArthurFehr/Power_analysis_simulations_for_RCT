
# Code to generate power analysis via simulations for the PIM program
# Idea here is to generate simulation for 2 treatment arms
# Ultimately, we want to see if we have enough power (given a sample size) to rank both treatment effects
## the smaller the difference between hypothetical treatment effects, the lower the power we have
## the greater the difference, the higher the power we have

###############
#### Setup ####
###############

# Set wd where to save graphs
# setwd('C:\\Users\\arthf\\Mestrado_Economia\\Clear\\PIM Program')

#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggsci)

############################################
#### Power Analysis for Multiple Treatments
############################################

rm(list=ls()) # clean environment

# ----- Define parameters ----- #

min_sample_size <- 100
max_sample_size <- 20000

sample_sizes <- seq(from = min_sample_size,
                    to   = max_sample_size,
                    by   = 1000)

sims <- 500   # nº of simulations to run for each treatment effect and each sample size

alpha <- 0.1  # one-tailed test at .05 level

# hypothetical treatment effects to be compared
taus_1 <- c(0.10, 0.10, 0.10)
taus_2 <- c(0.11, 0.15, 0.2) # must be same length as taus_1

# vectors to store power values for each sample size and each pair of treatment effects (MDEs)
## consider three scenarios to calculate power: 
### (i) at least one treatment found to be significant
### (ii) both treatments effects found to be significant
### (iii) both treatments effects found to be significant and we can rank them
power <- data.frame(
  atleastone = rep(NA, length(sample_sizes)*length(taus_1)),
  bothtreatments = rep(NA, length(sample_sizes)*length(taus_1)),
  fullranking = rep(NA, length(sample_sizes)*length(taus_1)),
  effect = c(rep(paste0('MDE1 = ', taus_1[1], ', MDE2 = ', taus_2[1]), length(sample_sizes)),
              rep(paste0('MDE1 = ', taus_1[2], ', MDE2 = ', taus_2[2]), length(sample_sizes)),
              rep(paste0('MDE1 = ', taus_1[3], ', MDE2 = ', taus_2[3]), length(sample_sizes)))
)

# --------------------- Run simulations --------------------- #

sample_number <- 1 # store number of sample being used to print during running time

# Loop to vary the treatment effects
for (k in 1:length(taus_1)){
  tau_1 = taus_1[k]
  tau_2 = taus_2[k]
  
  #### Loop to vary the sample size
  for (j in 1:length(sample_sizes)){
    N <- sample_sizes[j]
    # vector to save p-values
    p.T1vsC <- rep(NA, sims)
    p.T2vsC <- rep(NA, sims)
    p.T2vsT1 <- rep(NA, sims)
    # vector to save coefficients
    c.T1vsC <- rep(NA, sims)
    c.T2vsC <- rep(NA, sims)
    c.T2vsT1 <- rep(NA, sims)
    
    #### Loop to run through all simulations
    for (i in 1:sims){
      Y0 <-  rnorm(n=N, mean=0, sd=1) # assume potential outcome of untreated is normal(0, 1)
      Y1 <- Y0 + tau_1
      Y2 <- Y0 + tau_2
      Z.sim <- complete_ra(N=N, num_arms=3) # returns a N-vector with values T1, T2, T3
      Y.sim <- Y0*(Z.sim=="T3") + Y1*(Z.sim=="T1") + Y2*(Z.sim=="T2")
      # data frame with simulated Y values and treatment assignment
      frame.sim <- data.frame(Y.sim, Z.sim)
      # run OLS regressions of outcomes on treatment dummy
      fit.T1vsC.sim  <- lm(Y.sim ~ Z.sim=="T1", data=subset(frame.sim, Z.sim!="T2"))
      fit.T2vsC.sim  <- lm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim!="T1"))
      fit.T2vsT1.sim <- lm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim!="T3"))
      
      ### Need to capture coefficients and p-values (one-tailed tests, so signs are important)
      c.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,1]
      c.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,1]
      c.T2vsT1[i] <- summary(fit.T2vsT1.sim)$coefficients[2,1]
      
      p.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,4]
      p.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,4]
      p.T2vsT1[i] <- summary(fit.T2vsT1.sim)$coefficients[2,4]
      
    }
    
    # store simulations results for a given sample size
    ## at least one significant
    power[sample_number, 1] <- mean(c.T1vsC>0 & c.T2vsC>0 & (p.T1vsC < alpha/2 | p.T2vsC < alpha/2))
    ## both significant
    power[sample_number, 2] <- mean(c.T1vsC>0 & c.T2vsC>0 & p.T1vsC < alpha/2 & p.T2vsC < alpha/2)
    ## full ranking significant
    power[sample_number, 3] <- mean(c.T1vsC>0 & c.T2vsC>0 & c.T2vsT1 > 0 & p.T1vsC < alpha/2 & p.T2vsT1 < alpha/2)
    
    print(paste('Running sample', sample_number, 'from', length(sample_sizes)*length(taus_1), 'samples'))
    sample_number <- sample_number + 1
  }
  
}

# Create a data frame with the data to be plot
plot_data <- data.frame(
  sample_size = rep(sample_sizes, length(taus_1)),
  power) %>% 
  pivot_longer(cols = c('atleastone', 'bothtreatments', 'fullranking'),
               names_to = 'Power_type',
               values_to = 'Power_value')

# ---- Plot the graph ---- #

plot_data %>% 
  ggplot(aes(x = sample_size, y = Power_value, linetype = Power_type, color = effects)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black", size = 0.7) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, max(sample_sizes), by = 2000)) +
  labs(title = paste0("Power Analysis for Two Treatments"),
       x = "Sample sizes",
       y = "Power") +
  theme_classic() + 
  # scale_color_manual(name = 'Power type',
  #                    values = colors) +
  scale_color_lancet() + 
  scale_linetype_manual(name = 'Treatment effects',
                        values = c('dotted', 'dashed', 'solid'),
                        labels = c('At least one significant',
                                   'Both significant',
                                   'Full ranking significant')) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)) + 
  guides(color = guide_legend(title = 'Power type', override.aes = list(size = 3)),
         linetype = guide_legend(title = 'Treatment effects', override.aes = list(size = 0.8)))
  

# Save plot
ggsave('03_figures/Power analysis (2 treatments and 3 treatment effects).png',
       plot = last_plot(),
       width = 16,
       height = 8)

# Save the plot data
plot_data %>% saveRDS(file = '02_data/data_power_simulations.rds')
  


