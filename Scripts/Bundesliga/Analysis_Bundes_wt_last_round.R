#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
library(lubridate)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Bundesliga/Data_Bundesliga.R")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Double Poisson

double_pois <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Dixon-Coles

dixon_coles <- stan_foot(
  data = bundesliga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Negative Binomial

neg_bin <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam <- stan_foot(
  data = bundesliga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
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
  data = bundesliga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Double Poisson

double_pois_owen <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Negative Binomial

neg_bin_owen <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_owen <- stan_foot(
  data = bundesliga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam_owen <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  predict = 9,
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
  data = bundesliga,
  model = "dixon_coles",
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Double Poisson

double_pois_comm <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  predict = 9,
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
  data = bundesliga,
  model = "neg_bin",
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_comm <- stan_foot(
  data = bundesliga,
  model = "skellam",
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam_comm <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 23333332
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### KL Models                                                               ####


### Dixon-Coles

dixon_coles_kl <- stan_foot(
  data = bundesliga,
  model = "dixon_coles",
  predict = 9,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

### Double Poisson

double_pois_kl <- stan_foot(
  data = bundesliga,
  model = "double_pois",
  predict = 9,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Negative Binomial

neg_bin_kl <- stan_foot(
  data = bundesliga,
  model = "neg_bin",
  predict = 9,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Skellam

skellam_kl <- stan_foot(
  data = bundesliga,
  model = "skellam",
  predict = 9,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


### Zero Skellam

zero_skellam_kl <- stan_foot(
  data = bundesliga,
  model = "zero_infl_skellam",
  predict = 9,
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

max <- nrow(bundesliga)
min <- max - 8
bundesliga_test <- bundesliga[min:max, ]

comparison_bundes_wt_last_round <- compare_foot(source = list(
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
), test_data = bundesliga_test)

comparison_bundes_wt_last_round

save(comparison_bundes_wt_last_round,
  file = "RData/Bundesliga/comparison_bundes_wt_last_round.RData"
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
  test_data = bundesliga_test
)
print(score_metrics)

save(score_metrics, file = "RData/Bundesliga/score_metrics_bundes_wt_last_round.RData")


#   ____________________________________________________________________________
#   Convergence Stats                                                       ####

models <- list(
  dixon_coles   = dixon_coles_comm,
  double        = double_pois_comm,
  neg_bin       = neg_bin_comm,
  skellam       = skellam_comm,
  zero_skellam  = zero_skellam_comm
)

pars <- c("att", "def", "comm_sd_att", "comm_sd_def")

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
stats_bundes <- all_stats
save(stats_bundes, file = "RData/Bundesliga/stats_bundes_wt_last_round.RData")
View(stats_bundes)
