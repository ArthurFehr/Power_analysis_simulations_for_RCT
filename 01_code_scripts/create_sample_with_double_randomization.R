
create_sample_double_randomization <- function(treatment_effects = c(0.1),
                                               n_stratas = 10,
                                               n_treatments = 2,
                                               prob_treatment = 0.5, 
                                               observations_per_strata = 
                                                 sample(c(40, 60, 80, 100, 120),
                                                        size = n_stratas,
                                                        replace = TRUE,
                                                        prob = c(0.6, 0.2, 0.1, 0.06, 0.04)), 
                                               mean = 0, sd = 1, icc = 0) {
  
  strata <- data.table(strata_number = seq(n_stratas),
                       strata_observations = observations_per_strata)

  quantiles <- quantile(strata$strata_observations, probs = c(0.5))
  strata$quantile <- ifelse(strata$strata_observations <= quantiles, 1, 2)
  
  # Generate treatment group assignments
  strata[, treatment_group := randomizr::complete_ra(N = .N, prob = 0.5) + 1, 
         by = quantile]

  df_expanded <- data.table(
    strata_number = rep(seq(n_stratas),
                        times = observations_per_strata)
  )
  
  df_expanded <- merge(df_expanded, 
                       strata, by = "strata_number")
  
  df_expanded <- df_expanded[, by = .(treatment_group, strata_number),
                             treatment := randomizr::complete_ra(N = nrow(.SD),
                                                                 prob = prob_treatment)]
  # Creating potential outcome Y0
  mean_individual <- rep(mean, nrow(df_expanded))
  ## cluster shock
  epsilon_j <- rnorm(n = n_stratas,
                     mean = 0, sd = icc)
  epsilon_j <- rep(epsilon_j, 
                   times = observations_per_strata)
  # individual shock
  epsilon_ij <- rnorm(n = nrow(df_expanded),
                      mean = 0, sd = 1 - icc)
  
  Y0_outcomes <- mean + epsilon_j + epsilon_ij

  df_expanded[, Y0 := Y0_outcomes]
  
  return(df_expanded)
  
}
