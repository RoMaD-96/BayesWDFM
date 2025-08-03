#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)

#   ____________________________________________________________________________
#   Data                                                                    ####

premier_20_21 <- read_csv("Data/Premier_League/premier_20_21.csv")
premier_21_22 <- read_csv("Data/Premier_League/premier_21_22.csv")
premier_22_23 <- read_csv("Data/Premier_League/premier_22_23.csv")
premier_23_24 <- read_csv("Data/Premier_League/premier_23_24.csv")
premier_24_25 <- read_csv("Data/Premier_League/premier_24_25.csv")


# Add season column to each
premier_20_21$season <- 2021
premier_21_22$season <- 2022
premier_22_23$season <- 2023
premier_23_24$season <- 2024
premier_24_25$season <- 2025

premier <- rbind(
  premier_20_21[, c(107, 4:7)],
  premier_21_22[, c(107, 4:7)],
  premier_22_23[, c(107, 4:7)],
  premier_23_24[, c(107, 4:7)],
  premier_24_25[, c(121, 4:7)]
)

premier <- premier %>%
  group_by(season) %>%
  mutate(
    match_id = row_number(),
    half     = if_else(match_id <= n() / 2, 1L, 2L)
  ) %>%
  ungroup() %>%
  mutate(
    periods = (season - min(season)) * 2 + half
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

colnames(premier) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                             ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = premier,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = premier,
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
  data = premier,
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
  data = premier,
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
  data = premier,
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
  data = premier,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Owen (2011)                                              ####


### Bivariate Poisson

biv_pois_owen <- stan_foot(
  data = premier,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_owen <- stan_foot(
  data = premier,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_owen <- stan_foot(
  data = premier,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = premier,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_owen <- stan_foot(
  data = premier,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Bivariate Poisson

biv_pois_comm <- stan_foot(
  data = premier,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = premier,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = premier,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = premier,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_comm <- stan_foot(
  data = premier,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.55),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(premier)
min <- max-189
premier_test <- premier[min:max,]

comparison_premier <- compare_foot(source = list(
  biv_pois_comm = biv_pois_comm,
  biv_pois_egidi = biv_pois,
  biv_pois_owen = biv_pois_owen,
  diag_biv_pois_comm = diag_biv_pois_comm,
  diag_biv_pois_egidi = diag_biv_pois,
  diag_biv_pois_owen = diag_biv_pois_owen,
  double_pois_comm = double_pois_comm,
  double_pois_egidi = double_pois,
  double_pois_owen = double_pois_owen,
  neg_bin_comm = neg_bin_comm,
  neg_bin_egidi = neg_bin,
  neg_bin_owen = neg_bin_owen,
  skellam_comm = skellam_comm,
  skellam_egidi = skellam,
  skellam_owen = skellam_owen,
  zero_skellam_comm = zero_skellam_comm,
  zero_skellam_egidi = zero_skellam,
  zero_skellam_owen = zero_skellam_owen
), test_data = premier_test)

comparison_premier

save(comparison_premier, file = "comparison_premier.RData")
