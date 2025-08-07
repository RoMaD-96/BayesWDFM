#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(posterior)
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


#   ____________________________________________________________________________
#   Compute periods                                                          ####


la_liga <- la_liga %>%
  group_by(season) %>%
  mutate(
    match_id = row_number(),
    # first half = matches 1…n/2, second half = the rest
    half     = if_else(match_id <= n() / 2, 1L, 2L)
  ) %>%
  ungroup() %>%
  mutate(
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
  predict = 10,
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
min <- max - 9
la_liga_test <- la_liga[min:max, ]


comparison_liga_half_10 <- compare_foot(source = list(
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

comparison_liga_half_10

save(comparison_liga_half_10, file = "RData/La_Liga/comparison_liga_half_10.RData")


#   ____________________________________________________________________________
#   Abilities dataframe                                                     ####

get_ability_df <- function(stan_obj, variant_name, teams, type = "both") {
  draws <- stan_obj$fit$draws() %>% posterior::as_draws_rvars()
  att_arr <- posterior::draws_of(draws[["att"]])
  def_arr <- posterior::draws_of(draws[["def"]])

  all_teams <- unique(c(la_liga$home_team, la_liga$away_team))
  idx <- match(teams, all_teams)

  T_periods <- dim(att_arr)[2]
  N_teams <- length(idx)

  summarize_array <- function(arr) {
    list(
      med = apply(arr, c(2, 3), median)[, idx],
      lo  = apply(arr, c(2, 3), quantile, probs = 0.25)[, idx],
      hi  = apply(arr, c(2, 3), quantile, probs = 0.75)[, idx]
    )
  }
  a <- summarize_array(att_arr)
  d <- summarize_array(def_arr)

  make_df <- function(s, effect) {
    expand.grid(
      period = 1:T_periods,
      team = teams,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        mid = as.vector(s$med),
        lo = as.vector(s$lo),
        hi = as.vector(s$hi),
        effect = effect,
        variant = variant_name
      )
  }
  df_att <- make_df(a, "attack")
  df_def <- make_df(d, "defense")

  bind_rows(df_att, df_def)
}

my_teams <- c("Real Madrid", "Girona")

df_comm <- get_ability_df(diag_biv_pois_comm, "Comm", my_teams)
df_owen <- get_ability_df(diag_biv_pois_owen, "Owen", my_teams)
df_egidi <- get_ability_df(diag_biv_pois, "Egidi", my_teams)

df_abilities_liga_half_10 <- bind_rows(df_comm, df_owen, df_egidi)
save(df_abilities_liga_half_10, file = "RData/La_Liga/Abilities/df_abilities_liga_half_10.RData")

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
save(stats_liga, file = "RData/La_Liga/stats_liga_half_10.RData")
