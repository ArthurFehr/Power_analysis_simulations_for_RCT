
create_sample_double_randomization <- function(treatment_effects = c(0.1),
                                               n_stratas = 10,
                                               proportion_strata_in_treatment_one = 0.5,
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
  
  if (icc == 0) {
    
    Y0_outcomes <- rnorm(n = nrow(df_expanded),
                         mean = mean,
                         sd = sd)
    
  } else if (icc == 1) {

    Y0_outcomes <- fabricatr::fabricate(
      N = nrow(df_expanded),
      strata_id = df_expanded$strata_number,
      Y0 = draw_normal_icc(clusters = strata_id, sd = 0, sd_between = 1)
    )$Y0
    
  } else {
    
    sd_between = sqrt((icc * sd^2) / (1 - icc))
    
    Y0_outcomes <- fabricatr::fabricate(
      N = nrow(df_expanded),
      strata_id = df_expanded$strata_number,
      Y0 = draw_normal_icc(clusters = strata_id, sd = sd, sd_between = sd_between)
    )$Y0
    
  }
  
  df_expanded$Y0 <- Y0_outcomes
  
  return(df_expanded)
  
}

# create_sample_double_randomization <- function(treatment_effects = c(0.1),
#                                                n_stratas = 10,
#                                                proportion_strata_in_treatment_one = 0.5,
#                                                n_treatments = 2,
#                                                prob_treatment = 0.5,
#                                                observations_per_strata = 
#                                                  sample(c(40, 60, 80, 100, 120),
#                                                         size = n_stratas,
#                                                         replace = TRUE,
#                                                         prob = c(0.6, 0.2, 0.1, 0.06, 0.04)),
#                                                mean = 0, sd = 1) {
#   
#   # Generate strata information
#   strata <- data.frame(strata_number = seq(n_stratas),
#                        strata_observations = observations_per_strata)
#   quantiles <- quantile(strata$strata_observations, probs = c(0.5))
#   strata$quantile <- ifelse(strata$strata_observations <= quantiles, 1, 2)
#   
#   # Generate treatment group assignments
#   df_group_assignment <- data.table(strata_number = strata$strata_number)
#   df_group_assignment[, treatment_group := randomizr::complete_ra(N = .N, prob = 0.5) + 1, by = strata_number]
#   
#   # Expand the data
#   df_expanded <- merge(strata, df_group_assignment, by = "strata_number")
#   
#   # Simulate the sample
#   simulated_sample <- data.table()
#   
#   for (i in 1:n_treatments) {
#     data <- df_expanded[treatment_group == i]
#     
#     for (j in unique(data$strata_number)) {
#       data_strata <- data[strata_number == j]
#       
#       data_strata[, treatment := randomizr::complete_ra(N = .N, prob = prob_treatment)]
#       data_strata[, Y0 := rnorm(.N, mean = mean, sd = sd)]
#       
#       simulated_sample <- rbind(simulated_sample, data_strata)
#     }
#   }
#   
#   simulated_sample <- simulated_sample[order(strata_number)]
#   
#   return(simulated_sample)
# }

