#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
library(readxl)
library(lubridate) # For date handling


#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/La_Liga/Data_Liga_23_24.R")

#   ____________________________________________________________________________
#   Models                                                                  ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Egidi (2018)                                             ####


### Double Poisson

double_pois <- stan_foot(
  data = la_liga,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Dixon-Coles

dixon_coles <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)

### Negative Binomial

neg_bin <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Skellam

skellam <- stan_foot(
  data = la_liga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models Owen (2011)                                              ####


### Dixon-Coles

dixon_coles_owen <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)

### Double Poisson

double_pois_owen <- stan_foot(
  data = la_liga,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23233232
)


### Skellam

skellam_owen <- stan_foot(
  data = la_liga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Dixon-Coles

dixon_coles_comm <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 23232232
)

### Double Poisson

double_pois_comm <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23233232
)


### Negative Binomial

neg_bin_comm <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Skellam

skellam_comm <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 23232232
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 23232232
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### KL Models                                                               ####


### Dixon-Coles

dixon_coles_kl <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  predict = 190,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 23232232
)

### Double Poisson

double_pois_kl <- stan_foot(
  data = la_liga,
  model = "double_pois",
  predict = 190,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Negative Binomial

neg_bin_kl <- stan_foot(
  data = la_liga,
  model = "neg_bin",
  predict = 190,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Skellam

skellam_kl <- stan_foot(
  data = la_liga,
  model = "skellam",
  predict = 190,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### Zero Skellam

zero_skellam_kl <- stan_foot(
  data = la_liga,
  model = "zero_infl_skellam",
  predict = 190,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 23232232
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(la_liga)
min <- max - 189
la_liga_test <- la_liga[min:max, ]

comparison_la_liga_wt <- compare_foot(source = list(
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
), test_data = la_liga_test)

comparison_la_liga_wt

save(comparison_la_liga_wt, file = "RData/La_Liga/comparison_la_liga_wt_23_24.RData")



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
  test_data = la_liga_test
)
print(score_metrics)

save(score_metrics, file = "RData/La_Liga/score_metrics_la_liga_wt_23_24.RData")


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
stats_liga <- all_stats
save(stats_liga, file = "RData/La_Liga/stats_la_liga_wt_23_24.RData")
print(all_stats, n = 36)
