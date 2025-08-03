#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
#   ____________________________________________________________________________
#   Data                                                                    ####


bundesliga_20_21 <- read_csv("Data/Bundesliga/bundesliga_20_21.csv")
bundesliga_21_22 <- read_csv("Data/Bundesliga/bundesliga_21_22.csv")
bundesliga_22_23 <- read_csv("Data/Bundesliga/bundesliga_22_23.csv")
bundesliga_23_24 <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")
bundesliga_24_25 <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")

# Add season column to each

bundesliga_20_21$season <- 2021
bundesliga_21_22$season <- 2022
bundesliga_22_23$season <- 2023
bundesliga_23_24$season <- 2024
bundesliga_24_25$season <- 2025

bundesliga <- rbind(
  bundesliga_20_21[, c(106, 4:7)],
  bundesliga_21_22[, c(106, 4:7)],
  bundesliga_22_23[, c(106, 4:7)],
  bundesliga_23_24[, c(106, 4:7)],
  bundesliga_24_25[, c(120, 4:7)]
)

# bundesliga <- bundesliga %>%
#   group_by(season) %>%
#   mutate(
#     match_id = row_number(),
#     half     = if_else(match_id <= n() / 2, 1L, 2L)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     periods = (season - min(season)) * 2 + half
#   ) %>%
#   select(periods, HomeTeam, AwayTeam, FTHG, FTAG)
# 
 colnames(bundesliga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
# 

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = bundesliga,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = bundesliga,
  model = "diag_infl_biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Double Poisson

double_pois <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Negative Binomial

neg_bin <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Skellam

skellam <- stan_foot(
  data = bundesliga,
  model = "skellam",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  predict = 153,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Owen (2011)                                              ####


### Bivariate Poisson

biv_pois_owen <- stan_foot(
  data = bundesliga,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Diagonal Bivariate Poisson

diag_biv_pois_owen <- stan_foot(
  data = bundesliga,
  model = "diag_infl_biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Double Poisson

double_pois_owen <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Skellam

skellam_owen <- stan_foot(
  data = bundesliga,
  model = "skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Bivariate Poisson

biv_pois_comm <- stan_foot(
  data = bundesliga,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = bundesliga,
  model = "diag_infl_biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Skellam

skellam_comm <- stan_foot(
  data = bundesliga,
  model = "skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433, init = 0
)



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
     file = "all_models_bundes.RData")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(bundesliga)
min <- max-152
bundesliga_test <- bundesliga[min:max,]

comparison_bundes <- compare_foot(source = list(
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
), test_data = bundesliga_test)

comparison_bundes

save(comparison_bundes, file = "comparison_bundes.RData")



