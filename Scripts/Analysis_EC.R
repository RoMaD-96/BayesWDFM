#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("RData/StartingData_EC.RData")

ec_data_train <- ec_data_train[, -c(1, 6, 8)]

#   ____________________________________________________________________________
#   Bayesian Statistical Models                                             ####

##  ............................................................................
##  Group Stage                                                             ####

ngames_groupstage <- 36
ec_data_train_groupstage <- data.frame(
  home_team = c(
    "Germany", "Hungary", "Spain", "Italy", "Poland", "Slovenia", "Serbia", "Romania",
    "Belgium", "Austria", "Turkey", "Portugal", "Croatia", "Germany", "Scotland", "Slovenia",
    "Denmark", "Spain", "Slovakia", "Poland", "Netherlands", "Georgia", "Turkey", "Belgium",
    "Switzerland", "Scotland", "Albania", "Croatia", "Netherlands", "France", "England", "Denmark",
    "Slovakia", "Ukraine", "Georgia", "Czech Republic"
  ),
  away_team = c(
    "Scotland", "Switzerland", "Croatia", "Albania", "Netherlands", "Denmark", "England", "Ukraine",
    "Slovakia", "France", "Georgia", "Czech Republic", "Albania", "Hungary", "Switzerland", "Serbia",
    "England", "Italy", "Ukraine", "Austria", "France", "Czech Republic", "Portugal", "Romania", "Germany",
    "Hungary", "Spain", "Italy", "Austria", "Poland", "Slovenia", "Serbia", "Romania", "Belgium", "Portugal",
    "Turkey"
  ),
  home_score = c(5, 1, 3, 2, 1, 1, 0, 3, 0, 0, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 1, 0, 2, 1, 0, 0, 1, 2, 1, 0, 0, 1, 0, 2, 1),
  away_score = c(1, 3, 0, 1, 2, 1, 1, 0, 1, 1, 1, 1, 2, 0, 1, 1, 1, 0, 2, 3, 0, 1, 3, 0, 1, 1, 1, 1, 3, 1, 0, 0, 1, 0, 0, 2),
  period = rep(length(unique(ec_data_train$period)) + 1, ngames_groupstage)
)

ec_data_stan_GS <- rbind(ec_data_train, ec_data_train_groupstage)
ec_data_stan_GS <- ec_data_stan_GS[, c(5, 1, 2, 3, 4)]
colnames(ec_data_stan_GS) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Bivariate Poisson

biv_pois_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "biv_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)

print(biv_pois_GS)

### Diagonal Bivariate Poisson

diag_biv_pois_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "diag_infl_biv_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Double Poisson

double_pois_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "double_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Negative Binomial

neg_bin_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "neg_bin",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Skellam

skellam_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "skellam",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Zero Skellam

zero_skellam_GS <- stan_foot(
  data = ec_data_stan_GS,
  model = "zero_infl_skellam",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Weighted Dynamic Models                                                 ####


### Bivariate Poisson

biv_pois_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "biv_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.4),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Diagonal Bivariate Poisson

diag_biv_pois_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "diag_infl_biv_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.2),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Double Poisson

double_pois_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "double_pois",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.2),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Negative Binomial

neg_bin_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "neg_bin",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.2),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Skellam

skellam_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "skellam",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.2),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)


### Zero Skellam

zero_skellam_GS_comm <- stan_foot(
  data = ec_data_stan_GS,
  model = "zero_infl_skellam",
  predict = ngames_groupstage,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(prob_spike = 0.2),
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC"
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

ec_data_train_groupstage <- ec_data_train_groupstage[, c(5, 1, 2, 3, 4)]
colnames(ec_data_train_groupstage) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

comparison_GS <- compare_foot(source = list(
  biv_pois_comm = biv_pois_GS_comm,
  biv_pois = biv_pois_GS,
  diag_biv_pois_GS_comm = diag_biv_pois_GS_comm,
  diag_biv_pois_GS = diag_biv_pois_GS,
  double_pois_comm = double_pois_GS_comm,
  double_pois = double_pois_GS,
  neg_bin_GS_comm = neg_bin_GS_comm,
  neg_bin = neg_bin_GS,
  skellam_GS_comm = skellam_GS_comm,
  skellam_GS = skellam_GS,
  zero_skellam_GS_comm = zero_skellam_GS_comm,
  zero_skellam_GS = zero_skellam_GS
), test_data = ec_data_train_groupstage)

comparison_GS$metrics

save(comparison_GS, file = "comparison_GS.RData")





##  ............................................................................
##  Knockout Stage                                                          ####



### Train Test Knockout
ngames_knockout <- 15
ec_data_test <- data.frame(
  home_team = c("Switzerland", "Germany", "England", "Spain", "France", "Portugal", "Romania", "Austria",
                "Spain", "Portugal", "England", "Netherlands","Spain", "Netherlands", "Spain"),
  
  away_team = c("Italy", "Denmark", "Slovakia", "Georgia", "Belgium", "Slovenia", "Netherlands", "Turkey",
                "Germany", "France", "Switzerland", "Turkey", "France", "England", "England"),
  
  home_score = rep(NA, ngames_knockout),
  away_score = rep(NA, ngames_knockout),
  period = rep(length(unique(ec_data_train$period)) + 2, ngames_knockout))

ec_data_stan_KS <-rbind(ec_data_train,
                        ec_data_train_groupstage,
                        ec_data_test)
ec_data_stan_KS <- ec_data_stan_KS[,c(5,1:4)]


colnames(ec_data_stan_KS) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####



### Bivariate Poisson

biv_pois_KS <- stan_foot(
  data = ec_data_stan_KS,
  model = "biv_pois",
  predict = ngames_knockout,
  ranking = dyn_btd_KS,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)

print(biv_pois_KS)


biv_pois_KS_no_rank <- stan_foot(
  data = ec_data_stan_KS,
  model = "biv_pois",
  predict = ngames_knockout,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)

print(biv_pois_GS_no_rank)

foot_ability <- foot_abilities(biv_pois_KS_no_rank, ec_data_stan_KS,
                               type = "both",
                               teams = c("Italy", "Spain", "Croatia", "Germany", "England", "Austria")
)
foot_ability <- foot_ability +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 17), # facet titles
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16)
  )
ggsave(
  filename = "foot_ability.png", path = "Plots", plot = foot_ability,
  width = 22, height = 8, device = "png", dpi = 500
)

foot_prob_plot <- foot_prob(object = biv_pois_KS_no_rank, data = ec_data_test,
                            home_team = ec_data_test$home_team, away_team = ec_data_test$away_team)
foot_prob_plot <- foot_prob_plot$prob_plot +
  theme_bw(base_size = 22) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 17), # facet titles
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16)
  )

ggsave(
  filename = "foot_prob_plot.png", path = "Plots", plot = foot_prob_plot,
  width = 27, height = 11, device = "png", dpi = 500
)

### Double Poisson

double_pois_KS <- stan_foot(
  data = ec_data_stan_KS,
  model = "double_pois",
  predict = ngames_knockout,
  ranking = dyn_btd_KS,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)



double_pois_KS_no_rank <- stan_foot(
  data = ec_data_stan_KS,
  model = "double_pois",
  predict = ngames_knockout,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)


### Negative Binomial

neg_bin_KS <- stan_foot(
  data = ec_data_stan_KS,
  model = "neg_bin",
  predict = ngames_knockout,
  ranking = dyn_btd_KS,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)



neg_bin_KS_no_rank <- stan_foot(
  data = ec_data_stan_KS,
  model = "neg_bin",
  predict = ngames_knockout,
  dynamic_type = "seasonal",
  home_effect = FALSE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.90,
  max_treedepth = 15,
  method = "MCMC"
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Model Comparison                                                        ####

ec_data_test <- data.frame(
  home_team = c("Switzerland", "Germany", "England", "Spain", "France", "Portugal", "Romania", "Austria",
                "Spain", "Portugal", "England", "Netherlands","Spain", "Netherlands", "Spain"),
  
  away_team = c("Italy", "Denmark", "Slovakia", "Georgia", "Belgium", "Slovenia", "Netherlands", "Turkey",
                "Germany", "France", "Switzerland", "Turkey", "France", "England", "England"),
  
  home_goals = c(2, 2, 1, 4, 1, 0, 0, 1, 1, 0, 1, 2, 2, 2, 2),
  away_goals = c(0, 0, 1, 1, 0, 0, 3, 2, 1, 0, 1, 1, 1, 1, 1),
  periods = rep(length(unique(ec_data_train$period)) + 2, ngames_knockout))

colnames(ec_data_train_groupstage) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

compare_foot(source = list(
  biv_pois_btd = biv_pois_KS,
  biv_pois = biv_pois_KS_no_rank,
  double_pois_btd = double_pois_KS,
  double_pois = double_pois_KS_no_rank,
  neg_bin_btd = neg_bin_KS,
  neg_bin = neg_bin_KS_no_rank
), test_data = ec_data_test)
