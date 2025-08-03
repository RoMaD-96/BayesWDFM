#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
#   ____________________________________________________________________________
#   Data                                                                    ####

liga_20_21 <- read_csv("Data/Liga/liga_20_21.csv")
liga_21_22 <- read_csv("Data/Liga/liga_21_22.csv")
liga_22_23 <- read_csv("Data/Liga/liga_22_23.csv")
liga_23_24 <- read_csv("Data/Liga/liga_23_24.csv")
liga_24_25 <- read_csv("Data/Liga/liga_24_25.csv")

# Add season column to each
liga_20_21$season <- 2021
liga_21_22$season <- 2022
liga_22_23$season <- 2023
liga_23_24$season <- 2024
liga_24_25$season <- 2025

la_liga <- rbind(
  liga_20_21[, c(106, 4:7)],
  liga_21_22[, c(106, 4:7)],
  liga_22_23[, c(106, 4:7)],
  liga_23_24[, c(106, 4:7)],
  liga_24_25[, c(120, 4:7)]
)


colnames(la_liga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                              ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = la_liga,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Double Poisson

double_pois <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Negative Binomial

neg_bin <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Skellam

skellam <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Owen (2011)                                              ####


### Bivariate Poisson

biv_pois_owen <- stan_foot(
  data = la_liga,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Diagonal Bivariate Poisson

diag_biv_pois_owen <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Double Poisson

double_pois_owen <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Skellam

skellam_owen <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Bivariate Poisson

biv_pois_comm <- stan_foot(
  data = la_liga,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Skellam

skellam_comm <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4411
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(la_liga)
min <- max-189
la_liga_test <- la_liga[min:max,]


comparison_liga <- compare_foot(source = list(
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
), test_data = la_liga_test)

comparison_liga

save(comparison_liga, file = "comparison_liga.RData")

models_to_save <- list(
  biv_pois           = biv_pois,
  diag_biv_pois      = diag_biv_pois,
  double_pois        = double_pois,
  neg_bin            = neg_bin,
  skellam            = skellam,
  zero_skellam       = zero_skellam,
  biv_pois_owen      = biv_pois_owen,
  diag_biv_pois_owen = diag_biv_pois_owen,
  double_pois_owen   = double_pois_owen,
  neg_bin_owen       = neg_bin_owen,
  skellam_owen       = skellam_owen,
  zero_skellam_owen  = zero_skellam_owen,
  biv_pois_comm      = biv_pois_comm,
  diag_biv_pois_comm = diag_biv_pois_comm,
  double_pois_comm   = double_pois_comm,
  neg_bin_comm       = neg_bin_comm,
  skellam_comm       = skellam_comm,
  zero_skellam_comm  = zero_skellam_comm
)

save(list = names(models_to_save),
     file = "all_models_liga.RData")
