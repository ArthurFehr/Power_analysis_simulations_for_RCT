
# Code to generate power analysis via simulations for the PIM program
# Only one treatment arm considered but with 3 different treatment effects

###############
#### Setup ####
###############

# Set wd where to save graphs
# setwd('C:\\Users\\arthf\\Mestrado_Economia\\Clear\\PIM Program')

#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment
library(ggplot2)
library(data.table)
library(dplyr)

############################################
#### Power Analysis for Multiple Treatments
############################################

rm(list=ls()) # clean environment

# ----- Define parameters ----- #

min_sample_size <- 100
max_sample_size <- 6000

sample_sizes <- seq(from = min_sample_size,
                    to   = max_sample_size,
                    by   = 200)

sims <- 500   # nº of simulations to run for each treatment effect and each sample size

alpha <- 0.1  # one-tailed test at .05 level

# treatment effects
taus <- c(0.05, 0.10, 0.15) # these will be the minimum detectable effect (MDE) in our case

# vectors to store power value for each sample size
power <- data.frame(
  power_effect1 = rep(NA, length(sample_sizes)),
  power_effect2 = rep(NA, length(sample_sizes)),
  power_effect3 = rep(NA, length(sample_sizes))
  )

# colors for the graph
colors <- c('#BEBC73', '#95ADA5', '#CBD2BF')

# ---- Function to plot the graphs ---- #

plot_power_analysis <- function(data) {
  
  ggplot(data, aes(x = sample_sizes)) +
    # plot power for each treatment effect
    geom_line(aes(y = power_effect1, color = paste0('Effect = ', taus[1])), size = 2) +
    geom_line(aes(y = power_effect2, color = paste0('Effect = ', taus[2])), size = 2) +
    geom_line(aes(y = power_effect3, color = paste0('Effect = ', taus[3])), size = 2) +
    # creates horizontal line at power = 0.8
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq(0, max(sample_sizes), by = 1000)) +
    labs(title = paste0("Power Analysis"),
         x     = "Sample sizes",
         y     = "Power") +
    theme_classic() + 
    scale_color_manual(name   = '',
                       values = colors,
                       labels = c(paste0('Effect = ', taus[1]),
                                  paste0('Effect = ', taus[2]),
                                  paste0('Effect = ', taus[3]))) +
    theme(legend.position = 'bottom',
          legend.title    = element_blank(),
          legend.text     = element_text(size = 16),
          plot.title      = element_text(size = 24, hjust = 0.5),
          axis.title      = element_text(size = 20),
          axis.text       = element_text(size = 16)) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
}

# ----- Run simulations ----- #

sample_number <- 1 # store number of sample-simulation being used to print during running time

# Loop to vary the treatment effects
for (k in 1:length(taus)){
  tau_1 = taus[k]

  ## Loop to vary the sample size
  for (j in 1:length(sample_sizes)){
    N <- sample_sizes[j]
    # vector to save p-values (treatment vs control)
    p.T1vsC <- rep(NA, sims)
    # vector to save coefficients (treatment vs control)
    c.T1vsC <- rep(NA, sims)
    ### Loop to run through all simulations
    for (i in 1:sims){
      Y0 <-  rnorm(n=N, mean=0, sd=1) # assume potential outcome of untreated is normal(0, 1)
      Y1 <- Y0 + tau_1
      Z.sim <- complete_ra(N=N, num_arms=2) # returns a N-vector with values T1, T2
      # construct observed Y value
      ## assume 'T2' means not treated and 'T1' means treated
      Y.sim <- Y0*(Z.sim=="T2") + Y1*(Z.sim=="T1")
      # data frame with simulated observed Y values and treatment assignment
      frame.sim <- data.frame(Y.sim, Z.sim)
      # run OLS regressions of outcomes on treatment dummy
      fit.T1vsC.sim <- lm(Y.sim ~ Z.sim=="T1", data=frame.sim)

      ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)
      c.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,1]
      p.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,4]
      
    }
    
    # store simulations results for a given sample size
    power[j, k] <- mean(c.T1vsC>0 & p.T1vsC < alpha/2)

    print(paste('Running sample', sample_number, 'from', length(sample_sizes)*length(taus), 'samples'))
    sample_number <- sample_number + 1
    
  }
  
}

# Create a data frame with the data to be plot
plot_data <- data.frame(
  sample_sizes,
  power
)

# Save plot_data so don't need to rerun simulations to create the data
plot_data %>% 
  fwrite('02_data/data_for_single_treatment_arm.csv')

# Plot data
plot <- plot_power_analysis(plot_data)

# Save plot
ggsave(paste0('03_figures/Power analysis for single treatment.png'),
       plot = plot,
       width = 12,
       height = 8)
