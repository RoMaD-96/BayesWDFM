#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(dplyr)
library(footBayes)
library(readr)
library(tictoc)
library(purrr)

#   ____________________________________________________________________________
#   Data                                                                    ####

premier_20_21 <- read_csv("Data/Premier_League/premier_20_21.csv")
premier_21_22 <- read_csv("Data/Premier_League/premier_21_22.csv")
premier_22_23 <- read_csv("Data/Premier_League/premier_22_23.csv")
premier_23_24 <- read_csv("Data/Premier_League/premier_23_24.csv")
premier_24_25 <- read_csv("Data/Premier_League/premier_24_25.csv")

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


#   ____________________________________________________________________________
#   Simulations                                                             ####

model_runners <- list(
  biv_pois           = function(seed) stan_foot(data = premier, model = "biv_pois", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois      = function(seed) stan_foot(data = premier, model = "diag_infl_biv_pois", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois        = function(seed) stan_foot(data = premier, model = "double_pois", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin            = function(seed) stan_foot(data = premier, model = "neg_bin", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam            = function(seed) stan_foot(data = premier, model = "skellam", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam       = function(seed) stan_foot(data = premier, model = "zero_infl_skellam", predict = 30, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_owen      = function(seed) stan_foot(data = premier, model = "biv_pois", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_owen = function(seed) stan_foot(data = premier, model = "diag_infl_biv_pois", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_owen   = function(seed) stan_foot(data = premier, model = "double_pois", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_owen       = function(seed) stan_foot(data = premier, model = "neg_bin", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_owen       = function(seed) stan_foot(data = premier, model = "skellam", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_owen  = function(seed) stan_foot(data = premier, model = "zero_infl_skellam", predict = 30, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_comm      = function(seed) stan_foot(data = premier, model = "biv_pois", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_comm = function(seed) stan_foot(data = premier, model = "diag_infl_biv_pois", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_comm   = function(seed) stan_foot(data = premier, model = "double_pois", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_comm       = function(seed) stan_foot(data = premier, model = "neg_bin", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_comm       = function(seed) stan_foot(data = premier, model = "skellam", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_comm  = function(seed) stan_foot(data = premier, model = "zero_infl_skellam", predict = 30, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.55), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed)
)



time_results_premier_half_10 <- tibble(model = character(), run = integer(), elapsed = double())
base_seed <- 1000L

for (model_name in names(model_runners)) {
  tic.clearlog()
  for (i in seq_len(10)) {
    curr_seed <- base_seed + i
    set.seed(curr_seed)
    tic(model_name, quiet = TRUE)
    model_runners[[model_name]](curr_seed)
    toc(log = TRUE, quiet = TRUE)
  }
  logs <- tic.log(format = FALSE)
  times <- map_dbl(logs, ~ .x$toc - .x$tic)
  time_results_premier_half_10 <- bind_rows(
    time_results_premier_half_10,
    tibble(
      model = model_name,
      run = seq_along(times),
      elapsed = times
    )
  )
}