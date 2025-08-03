#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)

#   ____________________________________________________________________________
#   Data                                                                    ####

ligue_1_20_21 <- read_csv("Data/Ligue_1/ligue_1_20_21.csv")
ligue_1_21_22 <- read_csv("Data/Ligue_1/ligue_1_21_22.csv")
ligue_1_22_23 <- read_csv("Data/Ligue_1/ligue_1_22_23.csv")
ligue_1_23_24 <- read_csv("Data/Ligue_1/ligue_1_23_24.csv")
ligue_1_24_25 <- read_csv("Data/Ligue_1/ligue_1_24_25.csv")



# Add season column to each
ligue_1_20_21$season <- 2021
ligue_1_21_22$season <- 2022
ligue_1_22_23$season <- 2023
ligue_1_23_24$season <- 2024
ligue_1_24_25$season <- 2025

ligue_1 <- rbind(
  ligue_1_20_21[, c(106, 4:7)],
  ligue_1_21_22[, c(106, 4:7)],
  ligue_1_22_23[, c(106, 4:7)],
  ligue_1_23_24[, c(106, 4:7)],
  ligue_1_24_25[, c(120, 4:7)]
)




stats_summary_per <- ligue_1 %>%
  group_by(periods) %>%
  summarise(count = n(),
            avg_home_goals = mean(home_goals),
            avg_away_goals = mean(away_goals),
            rel_home_effect = mean(home_goals)/mean(away_goals) - 1,
            disp_index_h = var(home_goals)/mean(home_goals),
            disp_index_a = var(away_goals)/mean(away_goals),
            median_h_g = median(home_goals),
            median_a_g = median(away_goals),
            .groups = "drop")
print(stats_summary_per)


latest_season <- max(ligue_1$season, na.rm = TRUE)

ligue_1 <- ligue_1 %>%
  group_by(season) %>%
  mutate(
    match_id = row_number(),
    # first half = matches 1…n/2, second half = the rest
    half     = if_else(match_id <= n() / 2, 1L, 2L)
  ) %>%
  ungroup() %>%
  mutate(
    # force the newest season to be all “half 1”
    half    = if_else(season == latest_season, 1L, half),
    # compute a global period index
    periods = (season - min(season)) * 2 + half
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)


colnames(ligue_1) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                              ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
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
  data = ligue_1,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = ligue_1,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = ligue_1,
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
  data = ligue_1,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_comm <- stan_foot(
  data = ligue_1,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = ligue_1,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.3),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(ligue_1)
min <- max-189
ligue_1_test <- ligue_1[min:max,]

comparison_ligue_half <- compare_foot(source = list(
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
), test_data = ligue_1_test)

comparison_ligue_half



save(comparison_ligue_half, file = "comparison_ligue_1_half.RData")

