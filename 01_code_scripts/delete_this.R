
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(fixest)


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
                                               mean = 0, sd = 1) {
  
  strata <- data.frame(strata_number = seq(n_stratas),
                       strata_observations = observations_per_strata)
  quantiles <- quantile(strata$strata_observations, probs = c(0.5))
  strata$quantile <- lapply(strata$strata_observations,
                            function(x) {
                              if (x <= quantiles) 1 else 2 
                            })
  
  df_group_assignment <- data.frame()
  
  for (i in seq(length(quantiles) + 1)) {
    
    temp <- strata %>% 
      filter(quantile == i) 
    if (nrow(temp) > 0) {
      temp$treatment_group <-
        randomizr::complete_ra(N = nrow(temp),
                               prob = 0.5) + 1
      df_group_assignment <- rbind(df_group_assignment,
                                   temp)
    }
    
  }
  
  df_expanded <- data.frame(
    strata_number = rep(seq(n_stratas),
                        times = observations_per_strata)
  )
  
  df_expanded <- df_expanded %>% 
    left_join(df_group_assignment,
              by = c('strata_number')) %>% 
    select(strata_number, treatment_group, quantile)
  
  potential_outcomes <- data.frame()
  
  for (i in seq(n_treatments)) {
    
    data <- subset(df_expanded,
                   subset = treatment_group == i)
    
    for (j in unique(data$strata_number)) {
      
      data_strata <- subset(data,
                            subset = strata_number == j)
      
      data_strata[, 'treatment'] = randomizr::complete_ra(N = nrow(data_strata),
                                                          prob = prob_treatment)
      
      data_strata[, 'Y0'] =
        rnorm(n = nrow(data_strata),
              mean = mean,
              sd = sd)
      
      data_strata[, 'Y1'] =
        data_strata[, 'Y0'] +
        rep(treatment_effects,
            nrow(data_strata))
      
      data_strata[, 'Y'] =
        data_strata[, 'Y0'] * (1 - data_strata[, 'treatment']) +
        data_strata[, 'Y1'] * (data_strata[, 'treatment'])
      
      potential_outcomes <- rbind(potential_outcomes,
                                  data_strata)
      
    }
    
  }
  
  potential_outcomes <- potential_outcomes[order(potential_outcomes$strata_number), ]
  
  return(potential_outcomes)
  
}

n_stratas <- 210

observations_per_strata <- 
  lapply(n_stratas, function(x) {
    sample(c(40, 60, 80, 100, 120),
           size = x,
           replace = TRUE,
           prob = c(0.85, 0.1, 0.03, 0.01, 0.01))
  })

for (i in 1:10) {

  dt <- create_sample_double_randomization(
    n_stratas = 210,
    treatment_effects = 0.12,
    observations_per_strata = unlist(observations_per_strata)
  )
  
  #  *** Model especification ***
  model <- feols(Y ~ treatment, 
                 data = subset(dt,
                               subset = treatment_group == 1),
                 lean = TRUE)
  
  print(model)
  
  model <- feols(Y ~ treatment | quantile, 
                 data = subset(dt,
                               subset = treatment_group == 1),
                 lean = TRUE)
  
  print(model)

}