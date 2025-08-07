library(cmdstanr)
library(posterior)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
#   ____________________________________________________________________________
#   Data                                                                    ####
##  ............................................................................
##  Bundesliga                                                              ####

bundesliga_20_21 <- read_csv("Data/Bundesliga/bundesliga_20_21.csv")
bundesliga_21_22 <- read_csv("Data/Bundesliga/bundesliga_21_22.csv")
bundesliga_22_23 <- read_csv("Data/Bundesliga/bundesliga_22_23.csv")
bundesliga_23_24 <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")
bundesliga_24_25 <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")

# Add season column to each
bundesliga_20_21$season <- 2021
bundesliga_21_22$season <- 2022
bundesliga_22_23$season <- 2023
bundesliga_23_24$season <- 2024
bundesliga_24_25$season <- 2025

# Row‐bind keeping only season + the four match columns
bundesliga <- bind_rows(
  bundesliga_20_21 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_21_22 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_22_23 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_23_24 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_24_25 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG)
)

# Pre‐compute the newest season so we can “protect” it
latest_season <- max(bundesliga$season, na.rm = TRUE)

bundesliga <- bundesliga %>%
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


colnames(bundesliga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


##  ............................................................................
##  La Liga                                                                 ####

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


##  ............................................................................
##  EPL                                                                     ####

premier_20_21 <- read_csv("Data/Premier_League/premier_20_21.csv")
premier_21_22 <- read_csv("Data/Premier_League/premier_21_22.csv")
premier_22_23 <- read_csv("Data/Premier_League/premier_22_23.csv")
premier_23_24 <- read_csv("Data/Premier_League/premier_23_24.csv")
premier_24_25 <- read_csv("Data/Premier_League/premier_24_25.csv")


# Add season column to each
premier_20_21$season <- 2021
premier_21_22$season <- 2022
premier_22_23$season <- 2023
premier_23_24$season <- 2024
premier_24_25$season <- 2025

premier <- rbind(
  premier_20_21[, c(107, 4:7)],
  premier_21_22[, c(107, 4:7)],
  premier_22_23[, c(107, 4:7)],
  premier_23_24[, c(107, 4:7)],
  premier_24_25[, c(121, 4:7)]
)

premier <- premier %>%
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

colnames(premier) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


#   ____________________________________________________________________________
#   Last match scenario                                                     ####

# Bundesliga
biv_pois_bundes_scen_1 <- stan_foot(
  data = bundesliga,
  model = "biv_pois",
  predict = 9,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

# EPL
zero_skellam_premier_scen_1 <- stan_foot(
  data = premier,
  model = "skellam",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

# La Liga
diag_biv_pois_liga_scen_1 <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 10,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


##  ............................................................................
##  Last three matches scenario                                             ####

biv_pois_bundes_scen_2 <- stan_foot(
  data = bundesliga,
  model = "biv_pois",
  predict = 27,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

diag_biv_pois_premier_scen_2 <- stan_foot(
  data = premier,
  model = "diag_infl_biv_pois",
  predict = 30,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

diag_biv_pois_liga_scen_2 <- stan_foot(
  data = la_liga,
  model = "diag_infl_biv_pois",
  predict = 30,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)


##  ............................................................................
##  Last half-season scenario                                               ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####

# Row‐bind keeping only season + the four match columns
bundesliga <- bind_rows(
  bundesliga_20_21 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_21_22 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_22_23 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_23_24 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_24_25 %>% select(season, HomeTeam, AwayTeam, FTHG, FTAG)
)

# Pre‐compute the newest season so we can “protect” it
latest_season <- max(bundesliga$season, na.rm = TRUE)

bundesliga_half <- bundesliga  %>%
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

# 
colnames(bundesliga_half) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
# 


premier <- rbind(
  premier_20_21[, c(107, 4:7)],
  premier_21_22[, c(107, 4:7)],
  premier_22_23[, c(107, 4:7)],
  premier_23_24[, c(107, 4:7)],
  premier_24_25[, c(121, 4:7)]
)

latest_season <- max(premier$season, na.rm = TRUE)


premier_half <- premier %>%
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

colnames(premier_half) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

la_liga <- rbind(
  liga_20_21[, c(106, 4:7)],
  liga_21_22[, c(106, 4:7)],
  liga_22_23[, c(106, 4:7)],
  liga_23_24[, c(106, 4:7)],
  liga_24_25[, c(120, 4:7)]
)


latest_season <- max(la_liga$season, na.rm = TRUE)

la_liga_half <- la_liga %>%
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


colnames(la_liga_half) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Models                                                                  ####

biv_pois_bundes_scen_3 <- stan_foot(
  data = bundesliga_half,
  model = "biv_pois",
  predict = 153,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

biv_pois_premier_scen_3 <- stan_foot(
  data = premier_half,
  model = "biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

diag_biv_pois_liga_scen_3 <- stan_foot(
  data = la_liga_half,
  model = "diag_infl_biv_pois",
  predict = 190,
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike_prob = 0.01, slab = normal(0,5), spike = normal(100, 0.1)),
  home_effect = TRUE,
  iter_sampling = 1000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 4321
)

model_fits_by_scen <- list(
  "Final round" = list(
    Bundesliga = biv_pois_bundes_scen_1$fit,
    Premier   = zero_skellam_premier_scen_1$fit,
    LaLiga    = diag_biv_pois_liga_scen_1$fit
  ),
  "Last three rounds" = list(
    Bundesliga = biv_pois_bundes_scen_2$fit,
    Premier   = diag_biv_pois_premier_scen_2$fit,
    LaLiga    = diag_biv_pois_liga_scen_2$fit
  ),
  "Last second half" = list(
    Bundesliga = biv_pois_bundes_scen_3$fit,
    Premier   = biv_pois_premier_scen_3$fit,
    LaLiga    = diag_biv_pois_liga_scen_3$fit
  )
)

# 2. Helper to extract & tidy one fit (as you already have)
tidy_precisions <- function(fit, model_name) {
  as_draws_df(fit) %>%
    select(starts_with("comm_prec_att"), starts_with("comm_prec_def")) %>%
    pivot_longer(
      everything(),
      names_to       = c("param", "period"),
      names_pattern  = "(comm_prec_(?:att|def))\\[(\\d+)\\]",
      values_to      = "phi"
    ) %>%
    mutate(
      period = as.integer(period),
      type   = ifelse(grepl("att", param), "attack", "defense"),
      model  = model_name
    ) %>%
    select(model, type, period, phi)
}

# 3. Map over scenarios and models, bind into one big summary table
summary_all <- imap_dfr(model_fits_by_scen, function(fits, scen_label) {
  imap_dfr(fits, ~ tidy_precisions(.x, .y)) %>%
    mutate(scenario = scen_label)
}) %>%
  group_by(scenario, model, type, period) %>%
  summarize(
    mean_phi = mean(phi),
    lower_ci = quantile(phi, 0.025),
    upper_ci = quantile(phi, 0.975),
    .groups = "drop"
  ) %>%
  # ensure the scenarios appear in the right order:
  mutate(scenario = factor(scenario,
                           levels = c("Last second half","Last three rounds","Final round")
  ))


summary_1_10 <- summary_all %>%
  filter(period <= 10) %>%
  mutate(
    period = factor(period, levels = 1:10),
    model  = factor(model,
                    levels = c("Bundesliga","Premier","LaLiga"),
                    labels = c("Bundesliga","EPL","La Liga")
    )
  )


# 2. Plot with error bars + points, discrete x, and facet grid
comm_prec_plot <- ggplot(summary_1_10, aes(x = period, y = mean_phi, color = type)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  facet_grid(scenario ~ model, scales = "free_x") +
  scale_color_manual(
    name = "Ability",
    values = c(
      defense = "#377EB8",
      attack = "#cd1719"
    ),
    labels = c(
      attack  = "Attack",
      defense = "Defense"
    )) +
  labs(
    x     = "Periods",
    y     = expression(phi~values),
    color = "",
  ) +
  theme_bw() +
  theme(
    strip.placement = "outside", # format to look like title
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    legend.position = "top",
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
    strip.background = element_rect(fill = "grey95"),
  )

ggsave(
  filename = "comm_prec_plot.pdf", path = "Plots",
  plot = comm_prec_plot,
  width = 17, height = 12, device = "pdf", dpi = 500, useDingbats = FALSE
)
