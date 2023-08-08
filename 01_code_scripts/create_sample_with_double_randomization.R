
create_sample_double_randomization <- function(treatment_effects = c(0.1, 0.2),
                                               n_stratas = 10,
                                               proportion_strata_in_treatment_one = 0.1,
                                               n_treatments = 2,
                                               prob_treatment = 0.5, 
                                               observations_per_strata = rep(10, n_stratas), 
                                               mean = 0, sd = 1) {
  
  strata <- seq(n_stratas)
  treatment_group = randomizr::complete_ra(N = n_stratas,
                                           prob = 1 - proportion_strata_in_treatment_one) + 1
  group_assignment <- data.frame(strata = 
                                   rep(strata,
                                     times = observations_per_strata),
                                 treatment_group =
                                   rep(treatment_group,
                                     times = observations_per_strata))
  potential_outcomes <- data.frame()
  
  for (i in seq(n_treatments)) {
    
    data <- subset(group_assignment,
                   subset = treatment_group == i)
    
    for (j in unique(data$strata)) {
      
      data_strata <- subset(data,
                         subset = strata == j)
      
      data_strata[, 'treatment'] = randomizr::complete_ra(N = nrow(data_strata),
                                                          prob = prob_treatment)
      
      data_strata[, 'Y'] = 
        # Create Y0
        rnorm(n = nrow(data_strata),
              mean = mean,
              sd = sd) +
        # Add treatment effect if unit is assigned to treatment
        data_strata[, 'treatment'] * rep(treatment_effects[i],
                                  nrow(data_strata))
      
      potential_outcomes <- rbind(potential_outcomes,
                                  data_strata)
      
    }
    
  }
  
  potential_outcomes <- potential_outcomes[order(potential_outcomes$treatment_group,
                                                 potential_outcomes$strata), ]
  
  return(potential_outcomes)
  
}


