
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
source('01_code_scripts/create_sample.R')

# Set base path to save graphs
base_path <- 'C:\\Users\\arthf\\Mestrado_Economia\\Clear\\PIM_Program\\Power_analysis_simulations_for_RCT'

############################################
#### Power Analysis for Multiple Treatments
############################################

# ----- Define parameters ----- #

min_sample_size <- 1000
max_sample_size <- 2000
increment <- 500

sims <- 10   # nº of simulations to run for each treatment effect and each sample size

alpha <- 0.1  # one-tailed test at .05 level

# treatment effects
taus <- c(0.10) # these will be the minimum detectable effect (MDE) in our case

# proportion of control and treated units
prop_control_treat <- c(0.5, 0.5)

# colors for the graph
colors <- c('#BEBC73', '#95ADA5', '#CBD2BF')

# ----- Run simulations ----- #

power <- run_power_simulations()

# ----- Plot graphs ----- #

plot_data <- power

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
