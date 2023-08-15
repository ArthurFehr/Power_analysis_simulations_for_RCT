
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
library(foreach)
library(doParallel)

source('01_code_scripts/create_sample_with_double_randomization.R')
source('01_code_scripts/calculate_power.R')
source('01_code_scripts/calculate_mdes.R')

# ----- Define parameters ----- #

# number of stratas (municipalities)
n_stratas = 210
# observations in each strata
observations_per_strata <- 
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
    rep(220, 1))

power_level <- 0.8
alpha <- 0.1
min_effect <- 0
max_effect <- 3
epsilon <- 0.01
simulations <- 300
icc <- seq(0, 1, 0.2)

# ------ Run simulations ------ #

# Preallocate the result data.table
df_mde <- data.table(icc = rep(icc, times = length(n_stratas)),
                     n_strata = rep(n_stratas, each = length(icc)),
                     mde_results = numeric(length(n_stratas)))

df_mde[, c('mde_results') := lapply(icc,
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

df_mde[, c('mde', 'obs') := .(sapply(mde_results, function(x) x$mean_effect),
                                 sapply(mde_results, function(x) x$obs))]

# ----- Plot graph ----- #

df_mde %>% 
  ggplot(aes(x = icc, y = mde)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) + 
  geom_text(aes(label = round(mde, 2), vjust = -0.5)) +
  theme_classic() +
  xlab('ICC') +
  ylab('MDE') +
  ggtitle('MDE for 2 treatments (PIM 2 x PIM 1)') +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0,1))

ggsave('./03_figures/mde_2_treatments_comparisons.png')

