
run_mdes <- function(observations,
                     # n_stratas,
                     # observations_per_strata,
                     min_effect,
                     max_effect,
                     simulations,
                     epsilon,
                     alpha,
                     power_level) {
  
  power <- 0 
  
  while (abs(power - power_level) > epsilon) {
    
    mean_effect <- (min_effect + max_effect) / 2
    
    results <- calculate_power(mean_effect = 0.1,
                               alpha = 0.1,
                               simulations = 100,
                               power_level = 0.8,
                               epsilon = 0.01,
                               # icc = 1,
                               # n_stratas = n_stratas,
                               # obs_per_strata = observations_per_strata,
                               observations = 100)
    
    power <- results$power
    
    if (power < power_level - epsilon) {
      min_effect <- mean_effect
    } else {
      max_effect <- mean_effect
    }
    
    gc()
    
  }
  
  return(list(mean_effect = mean_effect, 
              obs = results$obs))
  
}
