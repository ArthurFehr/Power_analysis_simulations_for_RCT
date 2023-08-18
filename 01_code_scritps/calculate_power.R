
# Function to run regression and check if null is reject or not
run_regression <- function(data,
                           formula,
                           cluster = NULL,
                           alpha = 0.1) {
  
  model <- fixest::feols(as.formula(formula),
                         data = dt, 
                         cluster = cluster,
                         lean = TRUE)
  
  reject <- model$coeftable[2, 4] < alpha / 2
  
  return(reject)
  
}

dt <- data.table(
  Y = rnorm(100, 0, 2),
  t = c(rep(0, 50), rep(1, 50))
)
