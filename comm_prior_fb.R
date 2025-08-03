   library(dplyr)
   library(footBayes)
   data("italy")
   italy_2000 <- italy %>%
     dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
     filter(Season %in% as.character(2017:2021))
   
   colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
 
   # Example with fitted models
   fit_1 <- stan_foot(
     data = italy_2000,
     dynamic_type = "seasonal",
     parallel_chains = 4,
     model = "double_pois", predict = 190
   ) # Double Poisson model
   fit_2 <- stan_foot(
     data = italy_2000,
     model = "double_pois",
     dynamic_type = "seasonal",
     dynamic_weight = TRUE,
     dynamic_par = list(prob_spike = 0.4),
     parallel_chains = 4,
     predict = 190
   ) # Double Poisson model
 
   
   fit_3 <- stan_foot(
      data = italy_2000,
      parallel_chains = 4,
      model = "double_pois", predict = 190
   ) # Double Poisson model
   italy_2000_test <- italy_2000[1711:1900, ]
 
 
   compare_results_models <- compare_foot(
     source = list(
       double_poisson_dyn = fit_1,
       double_poisson_comm = fit_2,
       double_poisson_stat = fit_3
     ),
     test_data = italy_2000_test,
     metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
     conf_matrix = TRUE
   )
 
   print(compare_results_models)

   
   foot_abilities(fit_2, italy_2000) 
   foot_abilities(fit_1, italy_2000) 

   
   posterior1 <- fit_2$fit$draws(format = "matrix")
   mcmc_areas(posterior1, pars = c(
     "comm_prec_att[1]", "comm_prec_def[1]", "comm_prec_att[2]", "comm_prec_def[2]",
     "comm_prec_att[3]", "comm_prec_def[3]"
   )) +
     theme_bw()   
   library(bayesplot)
   mcmc_trace(fit_1$fit$draws(inc_warmup = FALSE))   
   