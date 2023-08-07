
# Code to generate power analysis via simulations for the PIM program
# Only one treatment arm considered but with 3 different treatment effects

rm(list=ls()) # clean environment

###############
#### Setup ####
###############

library(randomizr)    # randomizr package for complete random assignment
library(ggplot2)
library(data.table)
library(dplyr)

source('01_code_scripts/plot_function.R')

# Set base path to save graphs
base_path <- 'C:\\Users\\arthf\\Mestrado_Economia\\Clear\\PIM Program\\Power_analysis_simulations_for_RCT'

############################################
#### Power Analysis for Multiple Treatments
############################################

# ----- Define parameters ----- #

min_sample_size <- 1000
max_sample_size <- 6000
increment <- 1000

sims <- 50   # nº of simulations to run for each treatment effect and each sample size

alpha <- 0.1  # one-tailed test at .05 level

# treatment effects
taus <- c(0.05, 0.10, 0.15) # these will be the minimum detectable effect (MDE) in our case

# proportion of control and treated units
prop_control_treat <- c(0.5, 0.5)

# colors for the graph
colors <- c('#BEBC73', '#95ADA5', '#CBD2BF')

# ----- Implementation ----- #

# Create sample sizes
sample_sizes <- seq(from = min_sample_size,
                    to   = max_sample_size,
                    by   = increment)

# vectors to store power values for each sample size
power <- data.frame(matrix(NA, 
                           nrow = length(sample_sizes),
                           ncol = length(taus)))
colnames(power) <- paste0('power_effect', seq(length(taus)))



create_sample <- function(tau, prob_treatment = c(0.7, 0.3),
                          sample_size = 100, mean = 0, sd = 1) {

  num_arms <- length(prob_treatment) # number of treatment arms + control group

  Y0 <- rnorm(n = sample_size,
              mean = mean,
              sd = sd) # assume potential outcome of untreated is normal(0, 1)
  Y1 <- Y0 + tau
  Z.sim <- complete_ra(N = sample_size,
                       prob_each = prob_treatment,
                       num_arms = num_arms) # returns a N-vector with values T1, T2
  # construct observed Y value
  ## assume 'T1' means not treated and 'Tx' means treated (x > 1)
  Y.sim <- Y0*(Z.sim=="T1") + Y1*(Z.sim=="T2")
  # data frame with simulated observed Y values and treatment assignment
  frame.sim <- data.frame(Y.sim, Z.sim)
  
  return(frame.sim)
  
}

# ----- Run simulations ----- #

sample_number <- 1 # store number of sample-simulation pair 
                   # being used to print during running time

# Loop to vary the treatment effects
for (k in 1:length(taus)){
  tau = taus[k]

  ## Loop to vary the sample size
  for (j in 1:length(sample_sizes)){
    N <- sample_sizes[j]
    # vector to save p-values (treatment vs control)
    p.TvsC <- rep(NA, sims)
    # vector to save coefficients (treatment vs control)
    c.TvsC <- rep(NA, sims)
    ### Loop to run through all simulations
    for (i in 1:sims){
      simulated_sample <- create_sample(tau = tau,
                                        prob_treatment = prop_control_treat,
                                        sample_size = N)
      # run OLS regressions of outcomes on treatment dummy
      fit.TvsC.sim <- lm(Y.sim ~ Z.sim=="T2", data=simulated_sample)

      ### Need to capture coefficients and p-values (one-tailed tests, so signs are important)
      c.TvsC[i] <- summary(fit.TvsC.sim)$coefficients[2,1]
      p.TvsC[i] <- summary(fit.TvsC.sim)$coefficients[2,4]
      
    }
    
    # store simulations results for a given sample size
    power[j, k] <- mean(c.TvsC>0 & p.TvsC < alpha/2)

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
# plot_data %>% 
#   fwrite('02_data/data_for_single_treatment_arm.csv')

# Plot data
plot <- plot_power_analysis(plot_data)

# Save plot
# ggsave(paste0('03_figures/Power analysis for single treatment.png'),
#        plot = plot,
#        width = 12,
#        height = 8)
