#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(forcats)
library(footBayes)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Bundesliga/Data_Bundesliga.R")
source("Scripts/Premier_League/Data_Premier.R")
source("Scripts/La_Liga/Data_Liga.R")


#   ____________________________________________________________________________
#   Abilities dataframe                                                     ####

get_ability_df <- function(stan_obj, variant_name, teams, all_teams, type = "both") {
  draws <- stan_obj$fit$draws() |> posterior::as_draws_rvars()
  att_arr <- posterior::draws_of(draws[["att"]])
  def_arr <- posterior::draws_of(draws[["def"]])

  idx <- match(teams, all_teams)

  missing <- teams[is.na(idx)]
  if (length(missing) > 0) {
    stop(
      "These teams were not found in `all_teams`: ",
      paste(missing, collapse = ", "),
      "\n(You are probably matching against the wrong league/team list.)"
    )
  }

  T_periods <- dim(att_arr)[2]

  summarize_array <- function(arr) {
    list(
      med = apply(arr, c(2, 3), median)[, idx, drop = FALSE],
      lo  = apply(arr, c(2, 3), quantile, probs = 0.25)[, idx, drop = FALSE],
      hi  = apply(arr, c(2, 3), quantile, probs = 0.75)[, idx, drop = FALSE]
    )
  }

  a <- summarize_array(att_arr)
  d <- summarize_array(def_arr)

  make_df <- function(s, effect) {
    expand.grid(
      period = 1:T_periods,
      team = teams,
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(
        mid = as.vector(s$med),
        lo = as.vector(s$lo),
        hi = as.vector(s$hi),
        effect = effect,
        variant = variant_name
      )
  }

  df_att <- make_df(a, "attack")
  df_def <- make_df(d, "defense")

  out <- dplyr::bind_rows(df_att, df_def)

  if (type == "attack") out <- dplyr::filter(out, effect == "attack")
  if (type == "defense") out <- dplyr::filter(out, effect == "defense")

  out
}



#   ____________________________________________________________________________
#   Models                                                                  ####


##  ............................................................................
##  Bundesliga                                                              ####

skellam <- stan_foot(
  data = bundesliga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


skellam_owen <- stan_foot(
  data = bundesliga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


skellam_comm <- stan_foot(
  data = bundesliga,
  model = "skellam",
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)


skellam_kl <- stan_foot(
  data = bundesliga,
  model = "skellam",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


all_teams_bundes <- unique(c(bundesliga$home_team, bundesliga$away_team))
my_teams <- c("Bayern Munich", "Hoffenheim")

df_comm <- get_ability_df(skellam_comm, "Comm", my_teams, all_teams_bundes)
df_owen <- get_ability_df(skellam_owen, "Owen", my_teams, all_teams_bundes)
df_egidi <- get_ability_df(skellam, "Egidi", my_teams, all_teams_bundes)
df_kl <- get_ability_df(skellam_kl, "Koopman", my_teams, all_teams_bundes)

df_abilities_bundesliga <- bind_rows(df_comm, df_owen, df_egidi, df_kl)

##  ............................................................................
##  Premier League                                                          ####

double_pois <- stan_foot(
  data = premier,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


double_pois_owen <- stan_foot(
  data = premier,
  model = "double_pois",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


double_pois_comm <- stan_foot(
  data = premier,
  model = "double_pois",
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)


double_pois_kl <- stan_foot(
  data = premier,
  model = "double_pois",
  dynamic_type = "seasonal",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)

all_teams_epl <- unique(c(premier$home_team, premier$away_team))
my_teams <- c("Man City", "Man United")

df_comm <- get_ability_df(double_pois_comm, "Comm", my_teams, all_teams_epl)
df_owen <- get_ability_df(double_pois_owen, "Owen", my_teams, all_teams_epl)
df_egidi <- get_ability_df(double_pois, "Egidi", my_teams, all_teams_epl)
df_kl <- get_ability_df(double_pois_kl, "Koopman", my_teams, all_teams_epl)


df_abilities_premier <- bind_rows(df_comm, df_owen, df_egidi, df_kl)

##  ............................................................................
##  La Liga                                                                 ####

dixon_coles <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

dixon_coles_owen <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)


dixon_coles_comm <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)


dixon_coles_kl <- stan_foot(
  data = la_liga,
  model = "dixon_coles",
  prior_par = list(ability_sd = student_t(4, 0, 1)),
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4, init = 0,
  method = "MCMC", seed = 2333332
)

all_teams_liga <- unique(c(la_liga$home_team, la_liga$away_team))
my_teams <- c("Real Madrid", "Girona")

df_comm <- get_ability_df(dixon_coles_comm, "Comm", my_teams, all_teams_liga)
df_owen <- get_ability_df(dixon_coles_owen, "Owen", my_teams, all_teams_liga)
df_egidi <- get_ability_df(dixon_coles, "Egidi", my_teams, all_teams_liga)
df_kl <- get_ability_df(dixon_coles_kl, "Koopman", my_teams, all_teams_liga)

df_abilities_liga <- bind_rows(df_comm, df_owen, df_egidi, df_kl)

#   ____________________________________________________________________________
#   Data                                                                    ####

bundesliga <- df_abilities_bundesliga
liga <- df_abilities_liga
premier <- df_abilities_premier


combined <- bind_rows(bundesliga, premier, liga) %>%
  mutate(
    model_type = case_when(
      str_detect(variant, "Comm") ~ "Weighted Dynamic",
      str_detect(variant, "Egidi") ~ "Egidi et al. (2018)",
      str_detect(variant, "Owen") ~ "Owen (2011)",
      TRUE ~ "Koopman and Lit (2015)"
    )
  ) %>%
  mutate(
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)", "Koopman and Lit (2015)")
    )
  ) %>%
  mutate(
    league = case_when(
      team %in% c("Man City", "Man United") ~ "Premier League",
      team %in% c("Bayern Munich", "Hoffenheim") ~ "Bundesliga",
      team %in% c("Real Madrid", "Girona") ~ "La Liga",
      TRUE ~ NA_character_
    ),
    team = fct_relevel(
      team,
      "Bayern Munich", "Hoffenheim",
      "Man City", "Man United",
      "Real Madrid", "Girona"
    ),
    eff_league = interaction(league, effect, sep = "_")
  ) %>%
  select(-variant)

# Define period labels — compact single-line format
period_labels <- c(
  "1"  = "20/21-H1", "2"  = "20/21-H2",
  "3"  = "21/22-H1", "4"  = "21/22-H2",
  "5"  = "22/23-H1", "6"  = "22/23-H2",
  "7"  = "23/24-H1", "8"  = "23/24-H2",
  "9"  = "24/25-H1", "10" = "24/25-H2"
)

ability_plot <- ggplot(combined, aes(period, mid,
  color = effect,
  fill  = effect
)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
    alpha = 0.2,
    color = NA,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "grey50", size = 0.8) +
  geom_line(size = 1) +
  facet_grid(team ~ model_type, scales = "free_y") +
  scale_color_manual(
    name = "Ability",
    values = c(
      defense = "#377EB8",
      attack = "#cd1719"
    ),
    labels = c(
      attack  = "Attack",
      defense = "Defense"
    )
  ) +
  scale_fill_manual(
    values = c(
      defense = "#377EB8",
      attack = "#cd1719"
    ), guide = "none"
  ) +
  scale_x_continuous(
    breaks = 1:10,
    labels = period_labels
  ) +
  labs(
    x = "Season / Period",
    y = "Ability values"
  ) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    legend.position = "top",
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 11, angle = 30, hjust = 1),
    strip.background = element_rect(fill = "grey95"),
  )

ggsave(
  filename = "ability_plot.pdf", path = "Plots",
  plot = ability_plot,
  width = 20, height = 14, device = "pdf", dpi = 500, useDingbats = FALSE
)
