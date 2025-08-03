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

la_serie_a <- rbind(
  serie_a_20_21[, c(106, 4:7)],
  serie_a_21_22[, c(106, 4:7)],
  serie_a_22_23[, c(106, 4:7)],
  serie_a_23_24[, c(106, 4:7)],
  serie_a_24_25[, c(120, 4:7)]
)


#   ____________________________________________________________________________
#   Compute periods                                                          ####


la_serie_a <- la_serie_a %>%
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


colnames(la_serie_a) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                              ####


### Bivariate Poisson

biv_pois <- stan_foot(
  data = la_serie_a,
  model = "biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois <- stan_foot(
  data = la_serie_a,
  model = "diag_infl_biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois <- stan_foot(
  data = la_serie_a,
  model = "double_pois",
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin <- stan_foot(
  data = la_serie_a,
  model = "neg_bin",
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam <- stan_foot(
  data = la_serie_a,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = la_serie_a,
  model = "zero_infl_skellam",
  predict = 10,
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
  data = la_serie_a,
  model = "biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_owen <- stan_foot(
  data = la_serie_a,
  model = "diag_infl_biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_owen <- stan_foot(
  data = la_serie_a,
  model = "double_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = la_serie_a,
  model = "neg_bin",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_owen <- stan_foot(
  data = la_serie_a,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = la_serie_a,
  model = "zero_infl_skellam",
  predict = 10,
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
  data = la_serie_a,
  model = "biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.20),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Diagonal Bivariate Poisson

diag_biv_pois_comm <- stan_foot(
  data = la_serie_a,
  model = "diag_infl_biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.15),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Double Poisson

double_pois_comm <- stan_foot(
  data = la_serie_a,
  model = "double_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.15),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = la_serie_a,
  model = "neg_bin",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.15),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Skellam

skellam_comm <- stan_foot(
  data = la_serie_a,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.15),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = la_serie_a,
  model = "zero_infl_skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.15),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 433
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(la_serie_a)
min <- max-9
la_serie_a_test <- la_serie_a[min:max,]


comparison_serie_a_half_10 <- compare_foot(source = list(
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
), test_data = la_serie_a_test)

comparison_serie_a_half_10

save(comparison_serie_a_half_10, file = "comparison_serie_a_half_10.RData")
# 
# models_to_save <- list(
#   biv_pois           = biv_pois,
#   diag_biv_pois      = diag_biv_pois,
#   double_pois        = double_pois,
#   neg_bin            = neg_bin,
#   skellam            = skellam,
#   zero_skellam       = zero_skellam,
#   biv_pois_owen      = biv_pois_owen,
#   diag_biv_pois_owen = diag_biv_pois_owen,
#   double_pois_owen   = double_pois_owen,
#   neg_bin_owen       = neg_bin_owen,
#   skellam_owen       = skellam_owen,
#   zero_skellam_owen  = zero_skellam_owen,
#   biv_pois_comm      = biv_pois_comm,
#   diag_biv_pois_comm = diag_biv_pois_comm,
#   double_pois_comm   = double_pois_comm,
#   neg_bin_comm       = neg_bin_comm,
#   skellam_comm       = skellam_comm,
#   zero_skellam_comm  = zero_skellam_comm
# )
# 
# save(list = names(models_to_save),
#      file = "all_models_serie_a.RData")


library(dplyr)
library(tidyr)
library(ggplot2)


#   ____________________________________________________________________________
#   Ability plot comparison using the best model                            ####

get_ability_df <- function(stan_obj, variant_name, teams, type = "both") {
  draws   <- stan_obj$fit$draws() %>% posterior::as_draws_rvars()
  att_arr <- posterior::draws_of(draws[["att"]])  
  def_arr <- posterior::draws_of(draws[["def"]])
  
  all_teams <- unique(c(la_serie_a$home_team, la_serie_a$away_team))
  idx       <- match(teams, all_teams)
  
  T_periods <- dim(att_arr)[2]
  N_teams   <- length(idx)
  
  summarize_array <- function(arr) {
    list(
      med = apply(arr, c(2,3), median)[, idx],
      lo  = apply(arr, c(2,3), quantile, probs = 0.25)[, idx],
      hi  = apply(arr, c(2,3), quantile, probs = 0.75)[, idx]
    )
  }
  a <- summarize_array(att_arr)
  d <- summarize_array(def_arr)
  
  make_df <- function(s, effect) {
    expand.grid(
      period = 1:T_periods,
      team   = teams,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        mid    = as.vector(s$med),
        lo     = as.vector(s$lo),
        hi     = as.vector(s$hi),
        effect = effect,
        variant = variant_name
      )
  }
  df_att <- make_df(a, "attack")
  df_def <- make_df(d, "defense")
  
  bind_rows(df_att, df_def)
}

my_teams <- c("Real Madrid", "Girona")

df_comm  <- get_ability_df(diag_biv_pois_comm,      "Comm",  my_teams)
df_owen  <- get_ability_df(diag_biv_pois_owen,      "Owen",  my_teams)
df_egidi <- get_ability_df(diag_biv_pois,           "Egidi", my_teams)

df_abilities_serie_a_half_10   <- bind_rows(df_comm, df_owen, df_egidi)
save(df_abilities_serie_a_half_10, file = "RData/La_Serie_A/Abilities/df_abilities_serie_a_half_10.RData")



ggplot(df_abilities_serie_a_half_10, aes(x = period, y = mid, color = effect, fill = effect)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  facet_grid(team ~ variant, scales = "free_y") +
  labs(
    x = "Period",
    y = "Ability (median with 25–75% ribbon)",
    title = "Attack & Defense Trajectories by Model Variant and Team",
    color = "Effect",
    fill  = "Effect"
  ) +
  theme_bw() +
  theme(
    strip.text.x = element_text(face = "bold", size = 11),
    strip.text.y = element_text(face = "bold", size = 11),
    legend.position = "top"
  )

