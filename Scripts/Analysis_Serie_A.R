#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)

#   ____________________________________________________________________________
#   Data                                                                    ####

serie_a_20_21 <- read_csv("Data/Serie_A/serie_a_20_21.csv")
serie_a_21_22 <- read_csv("Data/Serie_A/serie_a_21_22.csv")
serie_a_22_23 <- read_csv("Data/Serie_A/serie_a_22_23.csv")
serie_a_23_24 <- read_csv("Data/Serie_A/serie_a_23_24.csv")
serie_a_24_25 <- read_csv("Data/Serie_A/serie_a_24_25.csv")


# Add season column to each
serie_a_20_21$season <- 2021
serie_a_21_22$season <- 2022
serie_a_22_23$season <- 2023
serie_a_23_24$season <- 2024
serie_a_24_25$season <- 2025

serie_a <- rbind(
  serie_a_20_21[, c(106, 4:7)],
  serie_a_21_22[, c(106, 4:7)],
  serie_a_22_23[, c(106, 4:7)],
  serie_a_23_24[, c(106, 4:7)],
  serie_a_24_25[, c(120, 4:7)]
)


colnames(serie_a) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = serie_a,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

print(biv_pois)

### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = serie_a,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois <- stan_foot(
  data = serie_a,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin <- stan_foot(
  data = serie_a,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam <- stan_foot(
  data = serie_a,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = serie_a,
  model = "zero_infl_skellam",
  predict = 190,
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
  data = serie_a,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = serie_a,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = serie_a,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = serie_a,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_comm <- stan_foot(
  data = serie_a,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = serie_a,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.6),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(serie_a)
min <- max-189
serie_a_test <- serie_a[min:max,]

comparison <- compare_foot(source = list(
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
), test_data = serie_a_test)

comparison$metrics

save(comparison, file = "comparison_serie-a.RData")



