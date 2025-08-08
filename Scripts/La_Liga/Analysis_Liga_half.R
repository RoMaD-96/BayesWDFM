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

la_liga <- bind_rows(
  liga_20_21 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_21_22 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_22_23 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_23_24 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_24_25 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG)
)

#   ____________________________________________________________________________
#   Compute periods                                                          ####

latest_season <- max(la_liga$season, na.rm = TRUE)

la_liga <- la_liga %>%
  group_by(season) %>%
  mutate(
    match_id = row_number(),
    half     = if_else(match_id <= n() / 2, 1L, 2L)
  ) %>%
  ungroup() %>%
  mutate(
    half    = if_else(season == latest_season, 1L, half),
    periods = (season - min(season)) * 2 + half
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  method = "MCMC", seed = 4321
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
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


### Skellam

skellam_comm <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0, 5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(la_liga)
min <- max - 189
la_liga_test <- la_liga[min:max, ]


comparison_liga_half <- compare_foot(source = list(
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

comparison_liga_half

save(comparison_liga_half, file = "comparison_liga_half.RData")

#   ____________________________________________________________________________
#   Convergence Stats                                                       ####

models <- list(
  biv_pois      = biv_pois_comm,
  diag_biv      = diag_biv_pois_comm,
  double        = double_pois_comm,
  neg_bin       = neg_bin_comm,
  skellam       = skellam_comm,
  zero_skellam  = zero_skellam_comm
)

# Parameters of interest
pars <- c("att", "def", "comm_sd_att", "comm_sd_def")

# Function to extract one fit’s grouped diagnostics
extract_diagnostics <- function(mod, model_name) {
  sum_df <- mod$fit$summary()

  sum_df %>%
    dplyr::filter(stringr::str_detect(
      variable,
      paste0("^(", paste(pars, collapse = "|"), ")")
    )) %>%
    dplyr::group_by(prefix = sub("\\[.*", "", variable)) %>%
    dplyr::summarise(
      mean_rhat = mean(rhat, na.rm = TRUE),
      mean_ess_bulk = mean(ess_bulk, na.rm = TRUE),
      mean_ess_tail = mean(ess_tail, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(model = model_name) %>%
    dplyr::select(model, prefix, everything())
}

# Map over all models and bind rows
all_stats <- purrr::imap_dfr(models, extract_diagnostics)
print(all_stats)
stats_liga <- all_stats
save(stats_liga, file = "RData/La_Liga/stats_liga_half.RData")
print(all_stats, n = 36)
