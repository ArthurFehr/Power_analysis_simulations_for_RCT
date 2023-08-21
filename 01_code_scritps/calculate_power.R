
# Function to run regression and check if null is reject or not
run_regression <- function(sample_size,
                           treatment_effect,
                           mean = 0, sd = 1,
                           formula = 'y~treatment',
                           cluster = NULL,
                           alpha = 0.1,
                           sample_simulator = NULL, ...) {
  
  # Generate simulated sample
  if (is.null(sample_simulator)) {
  
    sample <- simulate_sample(
      size = sample_size, 
      treatment_effect = treatment_effect,
      mean = mean, sd = sd
    )  
    
  } else {
    
    sample <- sample_simulator()
    
  }
  
  # Run regression to get rejection result
  model <- fixest::feols(as.formula(formula),
                         data = sample, 
                         cluster = cluster,
                         lean = TRUE)
  
  reject <- model$coeftable[2, 4] < alpha / 2
  
  return(reject)
  
}

# Function to calcular power through simulations
calculate_power <- function(simulations = 100, 
                            sample_size = 100, 
                            treatment_effect = 0.1,
                            mean = 0, sd = 1,
                            formula = 'y ~ treatment',
                            cluster = NULL, 
                            alpha = 0.1,
                            sample_simulator = NULL) {
  
  results <- data.table(
    power = rep(NA, simulations)
  )
  
  results[, c('power') := sapply(1:simulations,
                                 run_regression,
                                 sample_size = sample_size,
                                 treatment_effect = treatment_effect,
                                 cluster = cluster,
                                 mean = mean, sd = sd,
                                 formula = formula,
                                 alpha = alpha,
                                 sample_simulator = sample_simulator)]
  
  return(mean(results$power))
  
}

