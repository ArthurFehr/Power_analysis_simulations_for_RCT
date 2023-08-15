
run_mdes <- function(n_stratas,
                     observations_per_strata,
                     min_effect,
                     max_effect,
                     simulations,
                     epsilon,
                     alpha,
                     power_level,
                     icc) {
  
  power <- 0
  
  while (abs(power - power_level) > epsilon) {
    
    mean_effect <- (min_effect + max_effect) / 2
    
    power_list <- vector(mode = 'logical', length = simulations)
    obs_list <- vector(mode = 'numeric', length = simulations)
    
    results <- data.table(
      simulation_results = rep(NA, simulations),
      power = rep(NA, simulations),
      obs = rep(NA, simulations)
    )
    
    results[, c('simulation_results') := lapply(1:simulations,
                                                calculate_power,
                                                mean_effect = mean_effect,
                                                alpha = alpha,
                                                power_level = power_level,
                                                epsilon = epsilon,
                                                icc = icc)]
    
    results[, c('power', 'obs') := .(sapply(simulation_results, function(x) x$power),
                                     sapply(simulation_results, function(x) x$obs))]
    
    
    obs <- mean(results$obs)
    power <- mean(results$power)
    
    if (power < power_level - epsilon) {
      min_effect <- mean_effect
    } else {
      max_effect <- mean_effect
    }
    
    gc()
    
  }
  
  return(list(mean_effect = mean_effect, 
              obs = obs))
  
}