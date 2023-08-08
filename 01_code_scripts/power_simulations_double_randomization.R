
rm(list = ls())

source('01_code_scripts/create_sample_with_double_randomization.R')

# ----- Define parameters ----- #

n_treatments = 2
proportion_strata_in_treatment_one <- c(0.1, 0.3, 0.5)
proportion_strata_in_treatment_one <- c(0.5)
n_stratas = 2
observations_per_strata <- c(sample(c(20, 40, 60, 80, 100),
                                    size = n_stratas,
                                    replace = TRUE,
                                    prob = c(0.5, 0.20, 0.15, 0.1, 0.05)))
observations_per_strata <- c(1000, 3000)
proportion_treated_control <- seq(0.1, 0.9, 0.2)
proportion_treated_control <- 0.5

simulations <- 500 # nº of simulations to run for each treatment effect and each sample size
alpha <- 0.1 # one-tailed test at .05 level

# hypothetical treatment effects to be compared
# must be same length 
effects_treatment_1 <- c(0.10)
effects_treatment_2 <- c(0.10)

# -----  ----- #

entries <- 
  n_treatments *
  length(effects_treatment_1) *
  length(proportion_strata_in_treatment_one) *
  length(proportion_treated_control)

power <- data.frame(
  treatment_group = rep(seq(n_treatments), 
                        each = entries / n_treatments),
  treatment_effect = rep(c(effects_treatment_1, effects_treatment_2),
                         each = entries / (n_treatments * length(effects_treatment_1))),
  perc_strata_in_tg1 = c(rep(c(proportion_strata_in_treatment_one),
                           each = length(proportion_treated_control),
                           times = length(effects_treatment_1)),
                         rep(c(1 - proportion_strata_in_treatment_one),
                             each = length(proportion_treated_control),
                             times = length(effects_treatment_1))),
  perc_treated_control = rep(proportion_treated_control,
                             times = 2 * length(effects_treatment_1) *
                               length(proportion_strata_in_treatment_one)),
  power = rep(NA)
)

treatment_effects <- list(c(effects_treatment_1,
                          effects_treatment_2))

# ------ Run simulations ------ # 

iteration <- 1

for (k in treatment_effects) { 
  
  for (p in proportion_strata_in_treatment_one) {
    
    for (t in proportion_treated_control) {
      
      # Data frame to store simulations results
      simulations_output <- data.frame()
      simulation <- 1
      
      for (s in 1:simulations) {
      
      simulated_sample <- 
        create_sample_double_randomization(treatment_effects = k,
                                           n_stratas = n_stratas,
                                           proportion_strata_in_treatment_one = p,
                                           n_treatments = n_treatments,
                                           prob_treatment = t, 
                                           observations_per_strata = observations_per_strata, 
                                           mean = 0, sd = 1)

      # run OLS regressions of outcomes on treatment dummy and
      # get coefficients and p-values
      results <- lm(Y ~ treatment,
                    data = subset(simulated_sample,
                                  subset = treatment_group == 1))
      
      simulations_output[2*simulation - 1, 'treatment_group'] <- 1
      simulations_output[2*simulation - 1, 'coefficients'] <- summary(results)$coefficients[2, 1]
      simulations_output[2*simulation - 1, 'p_values'] <- summary(results)$coefficients[2, 4]
      
      results <- lm(Y ~ treatment,
                    data = subset(simulated_sample,
                                  subset = treatment_group == 2))
      
      simulations_output[2*simulation, 'treatment_group'] <- 2
      simulations_output[2*simulation, 'coefficients'] <- summary(results)$coefficients[2, 1]
      simulations_output[2*simulation, 'p_values'] <- summary(results)$coefficients[2, 4]
      
      simulation <- simulation + 1
      
    } 
    
      # store simulations results for a given sample size
      temp <- simulations_output[simulations_output$treatment_group == 1, ]
      power[iteration, 'power'] <- 
        mean(temp[, 'coefficients'] > 0 &
               temp[, 'p_values'] < alpha/2)
      print(mean(temp$coefficients))
      temp <- simulations_output[simulations_output$treatment_group == 2, ]
      power[nrow(power)/2 + iteration, 'power'] <- 
        mean(temp[, 'coefficients'] > 0 &
               temp[, 'p_values'] < alpha/2)
      print(mean(temp$coefficients))
      
    # Print progress
    # print(paste('Running sample', iteration, 'from', , 'samples'))
    iteration <- iteration + 1
    
    }
  }
}

power


