

create_sample <- function(treatment_effects = c(0.1),
                          n_stratas = 10,
                          prob_treatment = 0.5, 
                          observations = 100,
                          observations_per_strata = 
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
                              rep(220, 1)), 
                          mean = 0, sd = 1) {
  
  # observations_per_strata <- sample(
  #   observations_per_strata,
  #   size = n_stratas
  # )
  # 
  # strata <- data.table(strata_number = rep(seq(n_stratas),
  #                                          times = observations_per_strata))
  
  strata <- data.table(
    obs = seq(observations)
  )
  
  # Generate treatment group assignments
  strata[, treatment := randomizr::complete_ra(N = .N, prob = 0.5)]
  
  # Creating potential outcome Y0
  mean_individual <- rep(mean, nrow(strata))
  # individual shock
  epsilon_ij <- rnorm(n = nrow(strata),
                      mean = 0, sd = 1)
  
  Y0_outcomes <- mean + epsilon_ij
  
  strata[, Y0 := Y0_outcomes]
  
  return(strata)
  
}


