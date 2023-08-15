
run_regressions <- function(data, 
                            n_treatment_arms = 1) {
  
  results <- data.frame(coefficients = rep(NA, 1 + 2*(n_treatment_arms-1)),
                        p_values = rep(NA, 1 + 2*(n_treatment_arms-1)))
  
  if (n_treatment_arms == 1) {
    
    reg <- lm(outcome ~ as.factor(treatment_assignment), 
              data = data)
    results$coefficients <- summary(reg)$coefficients[2, 1]
    results$p_values <- summary(reg)$coefficients[2, 4]
    
  } else {
    
    reg <- lm(outcome ~ as.factor(treatment_assignment), 
              data = subset(data, treatment_assignment != 'T3'))
    results$coefficients[1] <- summary(reg)$coefficients[2,1] 
    results$p_values[1] <- summary(reg)$coefficients[2,4]

    reg <- lm(outcome ~ as.factor(treatment_assignment), 
              data = subset(data, treatment_assignment != 'T2'))
    results$coefficients[2] <- summary(reg)$coefficients[2,1] 
    results$p_values[2] <- summary(reg)$coefficients[2,4]
    
    reg <- lm(outcome ~ as.factor(treatment_assignment), 
              data = subset(data, treatment_assignment != 'T1'))
    results$coefficients[3] <- summary(reg)$coefficients[2,1] 
    results$p_values[3] <- summary(reg)$coefficients[2,4]
    
  }
  
  return(results)
  
}
