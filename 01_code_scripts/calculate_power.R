

# Function for power calculation
calculate_power <- function(mean_effect,
                            alpha = 0.1,
                            simulations = 100,
                            power_level = 0.8,
                            epsilon = 0.01,
                            icc = 0) {
  
  dt <- create_sample_double_randomization(
    n_stratas = n_stratas,
    observations_per_strata = unlist(observations_per_strata),
    icc = icc
  )
  
  dt <- dt[treatment == 1]
  dt <- dt[treatment_group == 1, treatment := 0]
  
  dt[, Y := Y0 + treatment * mean_effect]
  
  model <- fixest::feols(Y ~ treatment,
                         cluster = ~strata_number,
                         data = dt, lean = TRUE)
  
  obs <- nrow(dt)
  power <- model$coeftable[2, 4] < alpha / 2
  
  return(list(power = power, obs = obs))
  
}