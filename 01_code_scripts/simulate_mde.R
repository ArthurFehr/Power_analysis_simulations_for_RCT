
# Goal: calculate the mde for given sample sizes
# How it works?
#     First, you choose two initial treatment effects
#     Idea is that one should be small enough so that its power is under 80%
#     while the other has power larger than 80%
#     Then we take the mean of both effects and calculate its power
#     If it lies between (80% - epsilon, 80% + epsilon), then we found the MDE
#     Otherwise, we substitute one of the initial two treatment effects by this
#     mean effect and keep iteration until we find a mean effect with power
#     inside this range.
#     We repeat this process for each sample size that we have.

rm(list = ls())

library(fabricatr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(fixest)
library(ggsci)

source('01_code_scripts/create_sample_with_double_randomization.R')
source('01_code_scripts/calculate_power.R')
source('01_code_scripts/calculate_mdes.R')

# ----- Define parameters ----- #

# n_stratas = 210
# observations_per_strata <- c(rep(40, 182),
#                              rep(50, 5),
#                              rep(60, 7),
#                              rep(70, 2),
#                              rep(80, 4),
#                              rep(90, 2),
#                              rep(100, 2),
#                              rep(130, 1),
#                              rep(160, 1),
#                              rep(200, 3),
#                              rep(220, 1))

# number of stratas (municipalities)
total_stratas = 210

# observations in each strata
small_municipalities <- c(rep(40, 180))
medium_municipalities <- c(rep(40, 2),
                           rep(50, 5),
                           rep(60, 7),
                           rep(70, 2),
                           rep(80, 4))
large_municipalities <- c(rep(90, 2),
                          rep(100, 2),
                          rep(130, 1),
                          rep(160, 1),
                          rep(200, 3),
                          rep(220, 1))

sample_percentage <- c(1, 0.5, 0.2)

generate_samples <- function(municipalities, percentages) {
  lapply(percentages, function(p) 
    sample(municipalities, size = length(municipalities) * p))
}

small <- generate_samples(small_municipalities, sample_percentage)
medium <- generate_samples(medium_municipalities, sample_percentage)
large <- generate_samples(large_municipalities, sample_percentage)

total_observations <- Map(c, small, medium, large)
total_stratas <- lapply(sample_percentage, function(x) {total_stratas * x})

power_level <- 0.8
alpha <- 0.1
min_effect <- 0
max_effect <- 3
epsilon <- 0.01
simulations <- 500
icc <- seq(0, 1, 0.1)

# ------ Run simulations ------ #

# Preallocate the result data.table
n_stratas <- c(unlist(total_stratas[1]))
observations_per_strata <- c(unlist(total_observations[1]))
  
df_mde1 <- data.table(icc = rep(icc, times = length(n_stratas)),
                     n_strata = rep(n_stratas, each = length(icc)),
                     mde_results = numeric(length(n_stratas)))

df_mde1[, ('mde_results') := lapply(icc,
                                  function(x) {
                                    run_mdes(n_stratas,
                                             observations_per_strata,
                                             min_effect,
                                             max_effect,
                                             simulations,
                                             epsilon,
                                             alpha,
                                             power_level,
                                             icc = x) }
       )]

df_mde1[, c('mde', 'obs') := .(sapply(mde_results, function(x) x$mean_effect),
                                 sapply(mde_results, function(x) x$obs))]

df_mde1 <- data.table(
  icc = icc,
  mde = c(0.08026123, 0.08300781, 0.10986328, 0.14404297,
          0.17822266, 0.23437500, 0.26489258, 0.31250000,
          0.37109375, 0.40039062, 0.43457031),
  obs = 4950
)

n_stratas <- c(unlist(total_stratas[2]))
observations_per_strata <- c(unlist(total_observations[2]))

df_mde2 <- data.table(icc = rep(icc, times = length(n_stratas)),
                      n_strata = rep(n_stratas, each = length(icc)),
                      mde_results = numeric(length(n_stratas)))

df_mde2[, c('mde_results') := lapply(icc,
                                   function(x) {
                                     run_mdes(n_stratas,
                                              observations_per_strata,
                                              min_effect,
                                              max_effect,
                                              simulations,
                                              epsilon,
                                              alpha,
                                              power_level,
                                              icc = x) }
)]

df_mde2[, c('mde', 'obs') := .(sapply(mde_results, function(x) x$mean_effect),
                               sapply(mde_results, function(x) x$obs))]

df_mde2 <- data.table(
  icc = icc,
  mde = c(0.1113281, 0.1170959, 0.1552734, 0.2109375, 0.2578125,
          0.3046875, 0.3691406, 0.4335938, 0.5009766, 0.5651093,
          0.6049805),
  obs = 2475
)

n_stratas <- c(unlist(total_stratas[3]))
observations_per_strata <- c(unlist(total_observations[3]))

df_mde3 <- data.table(icc = rep(icc, times = length(n_stratas)),
                      n_strata = rep(n_stratas, each = length(icc)),
                      mde_results = numeric(length(n_stratas)))

df_mde3[, c('mde_results') := lapply(icc,
                                   function(x) {
                                     run_mdes(n_stratas,
                                              observations_per_strata,
                                              min_effect,
                                              max_effect,
                                              simulations,
                                              epsilon,
                                              alpha,
                                              power_level,
                                              icc = x) }
)]

df_mde3[, c('mde', 'obs') := .(sapply(mde_results, function(x) x$mean_effect),
                               sapply(mde_results, function(x) x$obs))]

df_mde3 <- data.table(
  icc = icc,
  mde = c(0.1816406, 0.1875000, 0.2460938, 0.3046875,
          0.3984375, 0.4687271, 0.5806732, 0.6796875,
          0.7500000, 0.8437500, 0.9256868),
  obs = 975
)

# ----- Plot graph ----- #

df_mde <- rbind(df_mde1, df_mde2, df_mde3) 

df_mde %>% 
  ggplot(aes(x = icc, y = mde, color = factor(obs))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) + 
  # geom_text(aes(label = round(mde, 2)), vjust = -0.3) +
  theme_classic() +
  xlab('ICC') +
  ylab('MDE') +
  ggtitle('MDE para 2 tratamentos (comparação PIM 2 x PIM 1)') +
  labs(color = 'Número de municípios') +
  theme(legend.position = 'bottom') +
  scale_color_lancet(labels = c('42', '105', '210')) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0,1))

ggsave('./03_figures/mde_2_treatments_comparisons.png', 
       width = 12, height = 7, units = 'in')

