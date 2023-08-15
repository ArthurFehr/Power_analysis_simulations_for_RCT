
rm(list = ls())

library(dplyr)
library(ggplot2)

source('01_code_scripts/create_sample_with_double_randomization.R')

# ----- Define parameters ----- #

n_treatments <- 2

proportion_strata_in_treatment_one <- c(0.3, 0.5, 0.7)

n_stratas = 10

observations_per_strata <- c(sample(c(40, 60, 80, 100, 120),
                                    size = n_stratas,
                                    replace = TRUE,
                                    prob = c(0.85, 0.1, 0.03, 0.01, 0.01)))

proportion_treated_control <- seq(0.1, 0.9, 0.4)


simulations <- 50 # nº of simulations to run for each treatment effect and each sample size
alpha <- 0.1 # one-tailed test at .05 level
power <- 0.8

# hypothetical treatment effects to be compared
# must be same length 
effects_treatment_1 <- c(0.13, 0.16)
effects_treatment_2 <- c(0.13, 0.16)

# -----  ----- #

entries <- 
  n_treatments *
  length(effects_treatment_1) *
  length(proportion_strata_in_treatment_one) *
  length(proportion_treated_control)

power <- data.frame(
  treatment_group = rep(seq(n_treatments), 
                        each = entries / n_treatments),
  treatment_effect = rep(c(effects_treatment_1, effects_treatment_2),
                         each = entries / (n_treatments * length(effects_treatment_1))),
  perc_strata_in_tg1 = c(rep(c(proportion_strata_in_treatment_one),
                           each = length(proportion_treated_control),
                           times = length(effects_treatment_1)),
                         rep(c(1 - proportion_strata_in_treatment_one),
                             each = length(proportion_treated_control),
                             times = length(effects_treatment_1))),
  perc_treated_control = rep(proportion_treated_control,
                             times = 2 * length(effects_treatment_1) *
                               length(proportion_strata_in_treatment_one)),
  power = rep(NA)
)

treatment_effects <- list(effects_treatment_1,
                          effects_treatment_2)

# ------ Run simulations ------ # 

iteration <- 1
iterations <- 
  length(unlist(treatment_effects[1])) * 
  length(proportion_strata_in_treatment_one) *
  length(proportion_treated_control)

for (k in seq(length(unlist(treatment_effects[1])))) {
  
  effects = sapply(treatment_effects, '[', k)

    for (p in proportion_strata_in_treatment_one) {
    
    for (t in proportion_treated_control) {
      
      # Data frame to store simulations results
      simulations_output <- data.frame()
      simulation <- 1
      time0 <- Sys.time()
      
      for (s in 1:simulations) {
      
      simulated_sample <- 
        create_sample_double_randomization(treatment_effects = effects,
                                           n_stratas = n_stratas,
                                           proportion_strata_in_treatment_one = p,
                                           n_treatments = n_treatments,
                                           prob_treatment = t, 
                                           observations_per_strata = observations_per_strata, 
                                           mean = 0, sd = 1)

      # run OLS regressions of outcomes on treatment dummy and
      # get coefficients and p-values
      results <- lm(Y ~ treatment,
                    data = subset(simulated_sample,
                                  subset = treatment_group == 1))
      
      simulations_output[2*simulation - 1, 'treatment_group'] <- 1
      simulations_output[2*simulation - 1, 'coefficients'] <- summary(results)$coefficients[2, 1]
      simulations_output[2*simulation - 1, 'p_values'] <- summary(results)$coefficients[2, 4]
      
      results <- lm(Y ~ treatment,
                    data = subset(simulated_sample,
                                  subset = treatment_group == 2))
      
      simulations_output[2*simulation, 'treatment_group'] <- 2
      simulations_output[2*simulation, 'coefficients'] <- summary(results)$coefficients[2, 1]
      simulations_output[2*simulation, 'p_values'] <- summary(results)$coefficients[2, 4]
      
      simulation <- simulation + 1
      
    } 
    
      # store simulations results for a given sample size
      temp <- simulations_output[simulations_output$treatment_group == 1, ]
      power[iteration, 'power'] <- 
        mean(temp[, 'coefficients'] > 0 &
               temp[, 'p_values'] < alpha/2)

      temp <- simulations_output[simulations_output$treatment_group == 2, ]
      power[nrow(power)/2 + iteration, 'power'] <- 
        mean(temp[, 'coefficients'] > 0 &
               temp[, 'p_values'] < alpha/2)

      
    # Print progress
    time1 <- Sys.time()
    print(paste0('Running process ', iteration, ' from ', iterations, ' processes (in ',
                round(time1 - time0, 1), ')'))
    iteration <- iteration + 1
    
    }
  }
}

# ------ Plot graphs ------ # 

power %>%
  filter(treatment_group == 1) %>%
  ggplot(aes(x = perc_treated_control,
             y = power,
             linetype = factor(perc_strata_in_tg1),
             # group = factor(treatment_group),
             color = factor(treatment_effect))) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  scale_linetype_manual(name = '% Strata in Treatment 1',
                        values = c('dotted', 'dotdash', 'solid'),
                        labels = c('30%',
                                   '50%',
                                   '70%')) +
  scale_color_manual(name = 'Treatment Effects',
                     values = c('blue', 'red', 'green', 'orange')) +
  guides(linetype = guide_legend(
    override.aes = list(size = 0.5)
  )) +
  scale_x_continuous(breaks = proportion_treated_control) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_classic() + 
  labs(title = paste0("Power Analysis"),
       x     = "Proportion Treated - Control",
       y     = "Power") +
  theme(legend.position = 'right',
        legend.title    = element_text(size = 16),
        legend.text     = element_text(size = 14),
        plot.title      = element_text(size = 24, hjust = 0.5),
        axis.title      = element_text(size = 20),
        axis.text       = element_text(size = 16))
  

  # plot power for each treatment effect
  geom_line(aes(y = power_effect1, color = paste0('Effect = ', taus[1])), linewidth = 2) +
  geom_line(aes(y = power_effect2, color = paste0('Effect = ', taus[2])), linewidth = 2) +
  geom_line(aes(y = power_effect3, color = paste0('Effect = ', taus[3])), linewidth = 2) +
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




