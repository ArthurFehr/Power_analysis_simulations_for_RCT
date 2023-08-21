
calculate_mde <- function(min_effect,
                          max_effect,
                          simulations = 100,
                          epsilon = 0.01,
                          alpha = 0.1,
                          power = 0.8) {
  
  power_result <- 0 
  
  while (abs(power_result - power) > epsilon) {
    
    mean_effect <- (min_effect + max_effect) / 2
    
    power_list <- vector(mode = 'logical', length = simulations)
    obs_list <- vector(mode = 'numeric', length = simulations)
    
    results <- data.table(
      power = rep(NA, simulations)
    )
    
    results[, c('power') := lapply(1:simulations,
                                   calculate_power,
                                   mean_effect = mean_effect,
                                   alpha = alpha)]
    
    power_result <- mean(results$power)
    
    if (power_result < power - epsilon) {
      min_effect <- mean_effect
    } else {
      max_effect <- mean_effect
    }
    
    gc()
    
  }
  
  return(mean_effect)
  
}