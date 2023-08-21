
simulate_sample <- function(size = 1000,
                            treatment_effect = 0.01,
                            mean = 0,
                            sd = 1) {
  
  y0 = rnorm(size, mean = mean, sd = sd)
  y1 = y0 + treatment_effect
  treatment = randomizr::complete_ra(N = size, prob = 0.5)
  y = y0 * (1 - treatment) + y1 * treatment
  
  df_sample <- data.table(
    y0 = y0,
    y1 = y1,
    treatment = treatment,
    y = y
  )
  
  return(df_sample)
  
}
