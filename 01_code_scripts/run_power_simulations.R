
source('01_code_scripts/create_sample.R')
source('01_code_scripts/run_regressions.R')

run_power_simulations <- function(treatment_effects = list(c(0.1),
                                                           c(0.11)),
                                  n_treatment_arms = 2, 
                                  simulations = 10,
                                  proportion_treated_control = c(1/3, 1/3, 1/3),
                                  min_sample_size = 1000, 
                                  max_sample_size = 6000, 
                                  increment = 1000) {
  
  sample_number <- 1 # store number of sample-simulation pair 
                     # being used to print during running time
  
  # Create sample sizes to use in the simulations
  sample_sizes <- seq(from = min_sample_size,
                      to   = max_sample_size,
                      by   = increment)
  
  # Create a data frame with treatment effects
  treatment_effects = data.frame(treatment_effects)
  names(treatment_effects) <- paste0("effect_", seq(treatment_effects))
  
  # Create a data frame to store power values for each sample size
  if (length(treatment_effects) > 1) {
    
    power <- data.frame(matrix(NA, 
                               nrow = length(sample_sizes)*3,
                               ncol = length(treatment_effects)),
                        type = c(rep('al_least_one', length(sample_sizes)),
                                 rep('both_treatments', length(sample_sizes)),
                                 rep('full_ranking', length(sample_sizes))),
                        sample_size = sample_sizes)
    colnames(power) <- c(paste0('power_effect', 
                                seq(length(treatment_effects))),
                         'type', 'sample_size') 
    
  } else {
    
    power <- data.frame(matrix(NA, 
                               nrow = length(sample_sizes),
                               ncol = nrow(treatment_effects)),
                        sample_size = sample_sizes)
    colnames(power) <- c(paste0('power_effect', seq(nrow(treatment_effects))),
                         'sample_size')

  }
  
  # Loop to vary the treatment effects
  for (k in seq(treatment_effects[, 1])) {
    
    tau = treatment_effects[k, ]
    
    ## Loop to vary the sample size
    for (j in 1:length(sample_sizes)) {
      # Sample size for current simulations
      N <- sample_sizes[j]
      # Data frame to store simulations results
      simulations_output <- data.frame()

      ### Loop to run through all simulations
      for (i in 1:simulations) {
        
        # Create sample to run current simulation
        simulated_sample <- create_sample(treatment_effects = tau,
                                          prob_treatment = proportion_treated_control,
                                          n_treatment_arms = n_treatment_arms)
        
        # run OLS regressions of outcomes on treatment dummy and
        # get coefficients and p-values
        results <- run_regressions(data = simulated_sample,
                                   n_treatment_arms = n_treatment_arms)
        
        simulations_output[i, 'coefficients'] <- results[1, 1]
        simulations_output[i, 'p_values'] <- results[1, 2]
        
        simulations_output <- rbind(simulations_output,
                                    results)
        
      }
      
      # store simulations results for a given sample size
      power[j, k] <- mean(simulations_output[, 'coefficients'] > 0 &
                            simulations_output[, 'p_values'] < alpha/2)
      
      # Print progress
      print(paste('Running sample', sample_number, 'from', length(sample_sizes)*length(taus), 'samples'))
      sample_number <- sample_number + 1
      
    }
    
  }
  
  return(power)
  
}
