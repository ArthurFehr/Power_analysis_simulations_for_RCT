
rm(list = ls())

library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(fixest)
library(ggsci)

source('01_code_scripts/create_sample_with_double_randomization.R')

# ----- Define parameters ----- #

n_stratas = seq(50, 210, 10)

observations_per_strata <- 
  lapply(n_stratas, function(x) {
    sample(c(40, 60, 80, 100, 120),
           size = x,
           replace = TRUE,
           prob = c(0.85, 0.1, 0.03, 0.01, 0.01))
  })

simulations <- 250 
alpha <- 0.1 # one-tailed test at .05 level

effects <- seq(0.08, 0.15, by = 0.005)

df_mde <- CJ(
  treatment_effect = effects,
  n_strata = n_stratas,
  simulation = seq_len(simulations)
)

simulation_number <- 1

for (k in effects) {
  
  for (i in seq(n_stratas)) {
    
    for (j in seq(simulations)) {
      
      time0 <- Sys.time()
      
      dt <- create_sample_double_randomization(
        n_stratas = n_stratas[i],
        treatment_effects = k,
        observations_per_strata = unlist(observations_per_strata[i])
      )
      
      #  *** Model especification ***
      model <- feols(Y ~ treatment, 
                     data = subset(dt,
                                   subset = treatment_group == 1),
                     lean = TRUE)

      reject_treatment1 <- model$coeftable[2, 4] < alpha / 2
      obs1 <- model$nobs
      
      # model <- feols(Y ~ treatment, 
      #                data = subset(dt,
      #                              subset = treatment_group == 2),
      #                lean = TRUE)
      # reject_treatment2 <- model$coeftable[2, 4] < alpha / 2
      # obs2 <- model$nobs
      
      # store results
      df_mde[treatment_effect == k &
               n_strata == n_stratas[i] &
               simulation == j, 'reject_treatment1'] = reject_treatment1
      # df_mde[n_strata == n_stratas[i] &
      #        simulation == j, 'reject_treatment2'] = reject_treatment2
      df_mde[treatment_effect == k &
               n_strata == n_stratas[i] &
               simulation == j, 'observations_treatment1'] = obs1
      # df_mde[n_strata == n_stratas[i] &
      #        simulation == j, 'observations_treatment2'] = obs2
      
      time1 <- Sys.time()
      print(paste0('Ran simulation ',
                   simulation_number,
                   '/',
                   length(effects) * length(n_stratas) * simulations,
                   ' in ',
                   round(time1 - time0, 2),
                   ' seconds'))
      simulation_number <- simulation_number + 1
      
    }
  }
}

df_mde %>% 
  fwrite('./02_data/df_mde.rds')

table <- df_mde[, by = .(treatment_effect, n_strata),
                lapply(.SD, mean),
                .SDcols = patterns("reject_|observations_")]


table %>% 
  ggplot() +
  geom_line(aes(x = observations_treatment1,
                y = reject_treatment1,
                color = factor(treatment_effect)),
            size = 1) +
  geom_hline(yintercept = 0.8, 
             linetype = "dashed", 
             color = "black", 
             size = 0.7) +
  scale_x_continuous(breaks = seq(0, 
                                  6000,
                                  by = 500)) +
  scale_y_continuous(breaks = seq(0, 
                                  1,
                                  by = 0.2)) +
  labs(title = paste0("Power Analysis"),
       x = "Sample size",
       y = "Power") +
  theme_classic()






