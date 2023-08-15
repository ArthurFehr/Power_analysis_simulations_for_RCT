
create_sample <- function(treatment_effects = c(0.1, 0.2),
                          n_clusters = 1,
                          prob_treatment = c(0.5, 0.5), 
                          n_treatment_arms = 2,
                          sample_size = 10, 
                          mean = 0, sd = 1) {
  
  # num_arms <- n_treatment_arms + 1 # number of treatment arms + control group
  
  # create a potential outcomes matrix
  # potential outcome for never treated is Y0
  # potential outcome for each treatment is named Y1, Y2, Y3, ...
  potential_outcomes <- data.frame(matrix(data = NA, 
                                          nrow = sample_size, 
                                          ncol = n_treatment_arms + 1))
  names(potential_outcomes) <- c('Y0', paste0('Y', seq(n_treatment_arms)))
  
  # Create potential outcomes for control group and each treatment
  # and stores them in the potential outcome data frame
  potential_outcomes[, 1] <- rnorm(n = sample_size,
                                   mean = mean,
                                   sd = sd) # assumes potential outcome of untreated is normal(0, 1)
  for (i in seq(n_treatment_arms)) {
    potential_outcomes[, 1 + i] <- potential_outcomes[, 1] + rep(treatment_effects[i][1], 
                                                                 sample_size)
  }
  
  # Create a n-vector of treatment assignments (values = T1, T2, ...) 
  treatment_assignments <- complete_ra(N = sample_size,
                                       prob_each = prob_treatment,
                                       num_arms = n_treatment_arms + 1) 

  # Construct observed Y value
  # 'T1' means not treated and 'Tx' means treated in treatment group x (x > 1)
  observed_outcome <- data.frame(outcome = rep(NA, sample_size),
                                 treatment_assignment = treatment_assignments)
  
  for (i in seq_len(sample_size)) {
    assignment <- observed_outcome$treatment_assignment[i] 
    potential_outcome_col <- paste0('Y', as.numeric(substr(assignment, 2, 2))-1)
    observed_outcome$outcome[i] <- potential_outcomes[i, potential_outcome_col]
  }
  
  return(observed_outcome)
  
}
