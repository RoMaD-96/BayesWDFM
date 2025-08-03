#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)

#   ____________________________________________________________________________
#   Data                                                                    ####

# Add season column to each
eredivisie_20_21$season <- 2021
eredivisie_21_22$season <- 2022
eredivisie_22_23$season <- 2023
eredivisie_23_24$season <- 2024
eredivisie_24_25$season <- 2025

eredivisie <- rbind(
  eredivisie_20_21[, c(106, 4:7)],
  eredivisie_21_22[, c(106, 4:7)],
  eredivisie_22_23[, c(106, 4:7)],
  eredivisie_23_24[, c(106, 4:7)],
  eredivisie_24_25[, c(120, 4:7)]
)


colnames(eredivisie) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = eredivisie,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

print(biv_pois)

### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = eredivisie,
  model = "diag_infl_biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois <- stan_foot(
  data = eredivisie,
  model = "double_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin <- stan_foot(
  data = eredivisie,
  model = "neg_bin",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam <- stan_foot(
  data = eredivisie,
  model = "skellam",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = eredivisie,
  model = "zero_infl_skellam",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Bivariate Poisson

biv_pois_comm <- stan_foot(
  data = eredivisie,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = eredivisie,
  model = "diag_infl_biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = eredivisie,
  model = "double_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = eredivisie,
  model = "neg_bin",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_comm <- stan_foot(
  data = eredivisie,
  model = "skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = eredivisie,
  model = "zero_infl_skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.2),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(eredivisie)
min <- max-152
eredivisie_test <- eredivisie[min:max,]

comparison_eredivisie <- compare_foot(source = list(
  biv_pois_comm = biv_pois_comm,
  biv_pois = biv_pois,
  diag_biv_pois_comm = diag_biv_pois_comm,
  diag_biv_pois = diag_biv_pois,
  double_pois_comm = double_pois_comm,
  double_pois = double_pois,
  neg_bin_comm = neg_bin_comm,
  neg_bin = neg_bin,
  skellam_comm = skellam_comm,
  skellam = skellam,
  zero_skellam_comm = zero_skellam_comm,
  zero_skellam = zero_skellam
), test_data = eredivisie_test)

comparison_eredivisie

save(comparison_eredivisie, file = "comparison_eredivisie.RData")



