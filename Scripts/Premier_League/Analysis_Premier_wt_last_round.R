#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
library(readxl)
library(lubridate)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Premier_League/Data_Premier.R")

#   ____________________________________________________________________________
#   Models                                                                  ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                             ####


### Double Poisson

double_pois <- stan_foot(
  data = premier,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Dixon-Coles

dixon_coles <- stan_foot(
  data = premier,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Negative Binomial

neg_bin <- stan_foot(
  data = premier,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam <- stan_foot(
  data = premier,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Owen (2011)                                              ####


### Dixon-Coles

dixon_coles_owen <- stan_foot(
  data = premier,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Double Poisson

double_pois_owen <- stan_foot(
  data = premier,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = premier,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_owen <- stan_foot(
  data = premier,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Dixon-Coles

dixon_coles_comm <- stan_foot(
  data = premier,
  model = "dixon_coles",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2332332
)

### Double Poisson

double_pois_comm <- stan_foot(
  data = premier,
  model = "double_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = premier,
  model = "neg_bin",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_comm <- stan_foot(
  data = premier,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23333332
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### KL Models                                                               ####


### Dixon-Coles

dixon_coles_kl <- stan_foot(
  data = premier,
  model = "dixon_coles",
  predict = 10,
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Double Poisson

double_pois_kl <- stan_foot(
  data = premier,
  model = "double_pois",
  predict = 10,
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Negative Binomial

neg_bin_kl <- stan_foot(
  data = premier,
  model = "neg_bin",
  predict = 10,
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_kl <- stan_foot(
  data = premier,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam_kl <- stan_foot(
  data = premier,
  model = "zero_infl_skellam",
  predict = 10,
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(premier)
min <- max - 9
premier_test <- premier[min:max, ]

comparison_premier_wt_last_round <- compare_foot(source = list(
  double_pois_comm = double_pois_comm,
  double_pois_egidi = double_pois,
  double_pois_owen = double_pois_owen,
  double_pois_kl = double_pois_kl,
  dixon_coles_comm = dixon_coles_comm,
  dixon_coles_egidi = dixon_coles,
  dixon_coles_owen = dixon_coles_owen,
  dixon_coles_kl = dixon_coles_kl,
  neg_bin_comm = neg_bin_comm,
  neg_bin_egidi = neg_bin,
  neg_bin_owen = neg_bin_owen,
  neg_bin_kl = neg_bin_kl,
  skellam_comm = skellam_comm,
  skellam_egidi = skellam,
  skellam_owen = skellam_owen,
  skellam_kl = skellam_kl,
  zero_skellam_comm = zero_skellam_comm,
  zero_skellam_egidi = zero_skellam,
  zero_skellam_owen = zero_skellam_owen,
  zero_skellam_kl = zero_skellam_kl
), test_data = premier_test)

comparison_premier_wt_last_round

save(comparison_premier_wt_last_round,
  file = "RData/Premier_League/comparison_premier_wt_last_round.RData"
)


score_metrics <- compare_foot_scores(
  source = list(
    double_pois_comm = double_pois_comm,
    double_pois_egidi = double_pois,
    double_pois_owen = double_pois_owen,
    double_pois_kl = double_pois_kl,
    dixon_coles_comm = dixon_coles_comm,
    dixon_coles_egidi = dixon_coles,
    dixon_coles_owen = dixon_coles_owen,
    dixon_coles_kl = dixon_coles_kl,
    neg_bin_comm = neg_bin_comm,
    neg_bin_egidi = neg_bin,
    neg_bin_owen = neg_bin_owen,
    neg_bin_kl = neg_bin_kl,
    skellam_comm = skellam_comm,
    skellam_egidi = skellam,
    skellam_owen = skellam_owen,
    skellam_kl = skellam_kl,
    zero_skellam_comm = zero_skellam_comm,
    zero_skellam_egidi = zero_skellam,
    zero_skellam_owen = zero_skellam_owen,
    zero_skellam_kl = zero_skellam_kl
  ),
  test_data = premier_test
)
print(score_metrics)

save(score_metrics,
  file = "RData/Premier_League/score_metrics_premier_wt_last_round.RData"
)


#   ____________________________________________________________________________
#   Convergence Stats                                                       ####

models <- list(
  dixon_coles   = dixon_coles_comm,
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
stats_premier <- all_stats
save(stats_premier, file = "RData/Premier_League/stats_premier_wt_last_round.RData")
print(all_stats, n = 40)

