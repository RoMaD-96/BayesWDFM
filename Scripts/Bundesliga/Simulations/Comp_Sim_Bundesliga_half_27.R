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

bundesliga_20_21 <- read_csv("Data/Bundesliga/bundesliga_20_21.csv")
bundesliga_21_22 <- read_csv("Data/Bundesliga/bundesliga_21_22.csv")
bundesliga_22_23 <- read_csv("Data/Bundesliga/bundesliga_22_23.csv")
bundesliga_23_24 <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")
bundesliga_24_25 <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")

bundesliga_20_21$season <- 2021
bundesliga_21_22$season <- 2022
bundesliga_22_23$season <- 2023
bundesliga_23_24$season <- 2024
bundesliga_24_25$season <- 2025

bundesliga <- bind_rows(
  bundesliga_20_21 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_21_22 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_22_23 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_23_24 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_24_25 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG)
)

#   ____________________________________________________________________________
#   Compute periods                                                          ####

bundesliga <- bundesliga %>%
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

colnames(bundesliga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


#   ____________________________________________________________________________
#   Simulations                                                             ####

model_runners <- list(
  biv_pois           = function(seed) stan_foot(data = bundesliga, model = "biv_pois", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois      = function(seed) stan_foot(data = bundesliga, model = "diag_infl_biv_pois", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois        = function(seed) stan_foot(data = bundesliga, model = "double_pois", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin            = function(seed) stan_foot(data = bundesliga, model = "neg_bin", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam            = function(seed) stan_foot(data = bundesliga, model = "skellam", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam       = function(seed) stan_foot(data = bundesliga, model = "zero_infl_skellam", predict = 27, dynamic_type = "seasonal", home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_owen      = function(seed) stan_foot(data = bundesliga, model = "biv_pois", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_owen = function(seed) stan_foot(data = bundesliga, model = "diag_infl_biv_pois", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_owen   = function(seed) stan_foot(data = bundesliga, model = "double_pois", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_owen       = function(seed) stan_foot(data = bundesliga, model = "neg_bin", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_owen       = function(seed) stan_foot(data = bundesliga, model = "skellam", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_owen  = function(seed) stan_foot(data = bundesliga, model = "zero_infl_skellam", predict = 27, dynamic_type = "seasonal", dynamic_par = list(common_sd = TRUE), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  biv_pois_comm      = function(seed) stan_foot(data = bundesliga, model = "biv_pois", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  diag_biv_pois_comm = function(seed) stan_foot(data = bundesliga, model = "diag_infl_biv_pois", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  double_pois_comm   = function(seed) stan_foot(data = bundesliga, model = "double_pois", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  neg_bin_comm       = function(seed) stan_foot(data = bundesliga, model = "neg_bin", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  skellam_comm       = function(seed) stan_foot(data = bundesliga, model = "skellam", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed),
  zero_skellam_comm  = function(seed) stan_foot(data = bundesliga, model = "zero_infl_skellam", predict = 27, dynamic_type = "seasonal", dynamic_weight = TRUE, dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)), home_effect = TRUE, iter_sampling = 1000, chains = 4, parallel_chains = 4, method = "MCMC", seed = seed)
)



time_results_bundes_half_27 <- tibble(model = character(), run = integer(), elapsed = double())
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
  time_results_bundes_half_27 <- bind_rows(
    time_results_bundes_half_27,
    tibble(
      model = model_name,
      run = seq_along(times),
      elapsed = times
    )
  )
}

save(time_results_bundes_half_27, file = "RData/Bundesliga/time_results_bundes_half_27.RData")
