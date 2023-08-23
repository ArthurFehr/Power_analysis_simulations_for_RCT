

# Function for power calculation
calculate_power <- function(mean_effect = 0.1,
                            alpha = 0.1,
                            simulations = 100,
                            power_level = 0.8,
                            epsilon = 0.01,
                            # icc = 1,
                            # n_stratas = n_stratas,
                            # obs_per_strata = observations_per_strata,
                            observations = 100) {
  
  results <- 
    replicate(simulations,
              {
                
                dt <- create_sample(
                  # n_stratas = n_stratas,
                  # observations_per_strata = obs_per_strata,
                  observations = observations
                )
                
                # dt <- dt[treatment == 1]
                # dt <- dt[treatment_group == 1, treatment := 0]
                
                dt[, Y := Y0 + treatment * mean_effect]
                
                model <- fixest::feols(Y ~ treatment,
                                       # cluster = ~strata_number,
                                       data = dt, lean = TRUE)
                
                sim_results <- {
                  obs <- nrow(dt)
                  power <- model$coeftable[2, 4] < alpha / 2
                  list(obs = obs, power = power)
                }
                
                sim_results
                
              }, simplify = FALSE)
  
  # dt <- create_sample_double_randomization(
  #   n_stratas = n_stratas,
  #   observations_per_strata = unlist(observations_per_strata),
  #   icc = icc
  # )
  
  dt_results <- data.table(
    obs = mean(unlist(lapply(results, function(results) results$obs))),
    power = mean(unlist(lapply(results, function(results) results$power)))
  )
  
  return(dt_results)
  
}

calculate_power()
