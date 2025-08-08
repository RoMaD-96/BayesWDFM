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

liga_20_21 <- read_csv("Data/Liga/liga_20_21.csv")
liga_21_22 <- read_csv("Data/Liga/liga_21_22.csv")
liga_22_23 <- read_csv("Data/Liga/liga_22_23.csv")
liga_23_24 <- read_csv("Data/Liga/liga_23_24.csv")
liga_24_25 <- read_csv("Data/Liga/liga_24_25.csv")

liga_20_21$season <- 2021
liga_21_22$season <- 2022
liga_22_23$season <- 2023
liga_23_24$season <- 2024
liga_24_25$season <- 2025

la_liga <- bind_rows(
  liga_20_21 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_21_22 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_22_23 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_23_24 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_24_25 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG)
)

#   ____________________________________________________________________________
#   Compute periods                                                          ####

la_liga <- la_liga %>%
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

colnames(la_liga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


#   ____________________________________________________________________________
#   Simulations                                                             ####

model_runners <- list(
  biv_pois           = function(seed) stan_foot(data = la_liga, model = "biv_pois", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois      = function(seed) stan_foot(data = la_liga, model = "diag_infl_biv_pois", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois        = function(seed) stan_foot(data = la_liga, model = "double_pois", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin            = function(seed) stan_foot(data = la_liga, model = "neg_bin", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam            = function(seed) stan_foot(data = la_liga, model = "skellam", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam       = function(seed) stan_foot(data = la_liga, model = "zero_infl_skellam", predict = 10, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_owen      = function(seed) stan_foot(data = la_liga, model = "biv_pois", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_owen = function(seed) stan_foot(data = la_liga, model = "diag_infl_biv_pois", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_owen   = function(seed) stan_foot(data = la_liga, model = "double_pois", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_owen       = function(seed) stan_foot(data = la_liga, model = "neg_bin", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_owen       = function(seed) stan_foot(data = la_liga, model = "skellam", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_owen  = function(seed) stan_foot(data = la_liga, model = "zero_infl_skellam", predict = 10, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_comm      = function(seed) stan_foot(data = la_liga, model = "biv_pois", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_comm = function(seed) stan_foot(data = la_liga, model = "diag_infl_biv_pois", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_comm   = function(seed) stan_foot(data = la_liga, model = "double_pois", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_comm       = function(seed) stan_foot(data = la_liga, model = "neg_bin", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_comm       = function(seed) stan_foot(data = la_liga, model = "skellam", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_comm  = function(seed) stan_foot(data = la_liga, model = "zero_infl_skellam", predict = 10, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed)
)



time_results_liga_half_10 <- tibble(model = character(), run = integer(), elapsed = double())
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
  time_results_liga_half_10 <- bind_rows(
    time_results_liga_half_10,
    tibble(
      model = model_name,
      run = seq_along(times),
      elapsed = times
    )
  )
}

save(time_results_liga_half_10, file = "RData/La_Liga/time_results_liga_half_10.RData")
