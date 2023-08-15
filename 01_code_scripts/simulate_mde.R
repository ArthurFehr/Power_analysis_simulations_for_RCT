
# Goal: calculate the mde for given sample sizes
# How it works?
#     First, you choose two initial treatment effects
#     Idea is that one should be small enough so that its power is under 80%
#     while the other has power larger than 80%
#     Then we take the mean of both effects and calculate its power
#     If it lies between (80% - epsilon, 80% + epsilon), then we found the MDE
#     Otherwise, we substitute one of the initial two treatment effects by this
#     mean effect and keep iteration until we find a mean effect with power
#     inside this range.
#     We repeat this process for each sample size that we have.

rm(list = ls())

library(fabricatr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(fixest)
library(ggsci)
library(foreach)
library(doParallel)

source('01_code_scripts/create_sample_with_double_randomization.R')

# 
num_cores <- 4
cl <- makeCluster(num_cores)

# ----- Define parameters ----- #

# number of stratas (municipalities)
n_stratas = seq(100, 210, 20)
n_stratas = 210
# observations in each strata
observations_per_strata <- 
  lapply(n_stratas, function(x) {
    sample(c(40, 60, 80, 100, 120),
           size = x,
           replace = TRUE,
           prob = c(0.85, 0.1, 0.03, 0.01, 0.01))
  })
observations_per_strata <- 
  c(rep(40, 182),
    rep(50, 5),
    rep(60, 7),
    rep(70, 2),
    rep(80, 4),
    rep(90, 2),
    rep(100, 2),
    rep(130, 1),
    rep(160, 1),
    rep(200, 3),
    rep(220, 1))

power_level <- 0.8
alpha <- 0.1
min_effect <- 0
max_effect <- 3
epsilon <- 0.01
simulations <- 1000
icc <- seq(0, 1, 0.2)

# --- Intermediary functions --- #

# Function for power calculation
calculate_power <- function(dt, mean_effect,
                            alpha = 0.1,
                            simulations = 100,
                            power_level = 0.8,
                            epsilon = 0.01) {

  power_list <- vector(mode = 'logical', length = simulations)
  obs_list <- vector(mode = 'numeric', length = simulations)

  for (j in seq(simulations)) {

    dt <- create_sample_double_randomization(
      n_stratas = n_stratas,
      observations_per_strata = unlist(observations_per_strata),
      ICC = icc[i]
    )

    dt <- dt[treatment == 1]
    dt <- dt[treatment_group == 1, treatment := 0]

    dt[, Y := Y0 + treatment * mean_effect]

    model <- fixest::feols(Y ~ treatment,
                           cluster = ~strata_number,
                   data = dt, lean = TRUE)

    power_list[j] <- model$coeftable[2, 4] < alpha / 2
    obs_list[j] <- nrow(dt)

  }

  obs <- mean(obs_list)
  power <- mean(power_list)

  return(list(power = power, obs = obs))

}

# ------ Run simulations ------ #

# Preallocate the result data.table
df_mde <- data.table(icc = rep(icc, times = length(n_stratas)),
                     n_strata = rep(n_stratas, each = length(icc)),
                     mde = numeric(length(n_stratas)), 
                     observations = numeric(length(n_stratas)))

time0 <- Sys.time()
registerDoParallel(cl)

results <- foreach(i = seq_along(icc),
                   .combine = rbind) %dopar% {
                     
                     library(data.table)
                     library(fabricatr)
                     
                     power <- 0
                     
                     while (abs(power - power_level) > epsilon) {
                       
                       mean_effect <- (min_effect + max_effect) / 2
                       
                       result <- calculate_power(dt, mean_effect, alpha,
                                                 simulations, power_level,
                                                 epsilon)
                       
                       power <- result$power
                       obs <- result$obs
                       
                       if (power < power_level - epsilon) {
                         min_effect <- mean_effect
                       } else {
                         max_effect <- mean_effect
                       }
                       
                     }
                     
                     return(list(
                       icc = icc[i],
                       mean_effect = mean_effect,
                       obs = obs
                     ))
                     
                   }

time1 <- Sys.time()
print(time1 - time0)

stopCluster(cl)
registerDoSEQ()

for (i in seq_along(icc)) {
  
  df_mde[i, c('mde', 'observations')] <- 
    unlist(results[i][c('mean_effect', 'obs')])
  
}


