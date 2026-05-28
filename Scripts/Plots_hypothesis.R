#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(patchwork)
library(posterior)
library(footBayes)

#   ____________________________________________________________________________
#   Data Loading                                                            ####

# Premier League
premier_20_21 <- read_csv("Data/Premier_League/premier_20_21.csv")
premier_21_22 <- read_csv("Data/Premier_League/premier_21_22.csv")
premier_22_23 <- read_csv("Data/Premier_League/premier_22_23.csv")
premier_23_24 <- read_csv("Data/Premier_League/premier_23_24.csv")
premier_24_25 <- read_csv("Data/Premier_League/premier_24_25.csv")

premier_20_21$season <- 2021
premier_21_22$season <- 2022
premier_22_23$season <- 2023
premier_23_24$season <- 2024
premier_24_25$season <- 2025

premier <- bind_rows(
  premier_20_21 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  premier_21_22 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  premier_22_23 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  premier_23_24 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  premier_24_25 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG)
) %>%
  mutate(Date = dmy(Date), league = "EPL")

# Bundesliga
bundesliga_20_21 <- read_csv("Data/Bundesliga/bundesliga_20_21.csv")
bundesliga_21_22 <- read_csv("Data/Bundesliga/bundesliga_21_22.csv")
bundesliga_22_23 <- read_csv("Data/Bundesliga/bundesliga_22_23.csv")
bundesliga_23_24 <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")
bundesliga_24_25 <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")

bundesliga_20_21$season <- 2021
bundesliga_21_22$season <- 2022
bundesliga_22_23$season <- 2023
bundesliga_23_24$season <- 2024
bundesliga_24_25$season <- 2025

bundesliga <- bind_rows(
  bundesliga_20_21 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_21_22 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_22_23 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_23_24 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  bundesliga_24_25 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG)
) %>%
  mutate(Date = dmy(Date), league = "Bundesliga")

# La Liga
liga_20_21 <- read_csv("Data/Liga/liga_20_21.csv")
liga_21_22 <- read_csv("Data/Liga/liga_21_22.csv")
liga_22_23 <- read_csv("Data/Liga/liga_22_23.csv")
liga_23_24 <- read_csv("Data/Liga/liga_23_24.csv")
liga_24_25 <- read_csv("Data/Liga/liga_24_25.csv")

liga_20_21$season <- 2021
liga_21_22$season <- 2022
liga_22_23$season <- 2023
liga_23_24$season <- 2024
liga_24_25$season <- 2025

la_liga <- bind_rows(
  liga_20_21 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_21_22 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_22_23 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_23_24 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_24_25 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG)
) %>%
  mutate(Date = dmy(Date), league = "La Liga")

# Combine all leagues
all_data <- bind_rows(premier, bundesliga, la_liga)

# Set factor levels: Bundesliga first, EPL second, La Liga last
all_data$league <- factor(all_data$league,
  levels = c("Bundesliga", "EPL", "La Liga")
)

#   ____________________________________________________________________________
#   Calculate all metrics                                                   ####

# Home Advantage
home_advantage <- all_data %>%
  group_by(league, season) %>%
  summarise(
    value = mean(FTHG - FTAG),
    .groups = "drop"
  ) %>%
  mutate(metric = "Home Advantage", type = "Single")

# Dispersion - Home and Away separately
dispersion <- all_data %>%
  group_by(league, season) %>%
  summarise(
    dispersion_home = var(FTHG) / mean(FTHG),
    dispersion_away = var(FTAG) / mean(FTAG),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(dispersion_home, dispersion_away),
    names_to = "type", values_to = "value"
  ) %>%
  mutate(
    metric = "Dispersion",
    type = ifelse(type == "dispersion_home", "Home", "Away")
  )

# Correlation
correlation <- all_data %>%
  group_by(league, season) %>%
  summarise(
    value = cor(FTHG, FTAG),
    .groups = "drop"
  ) %>%
  mutate(metric = "Correlation", type = "Single")

# Combine all metrics
all_metrics <- bind_rows(home_advantage, dispersion, correlation)

all_metrics$metric <- factor(all_metrics$metric,
  levels = c("Home Advantage", "Dispersion", "Correlation")
)

# Ensure league factor is preserved in all_metrics
all_metrics$league <- factor(all_metrics$league,
  levels = c("Bundesliga", "EPL", "La Liga")
)

#   ____________________________________________________________________________
#   Prepare data subsets                                                    ####

metrics_home <- all_metrics %>% filter(metric == "Home Advantage")
metrics_disp <- all_metrics %>% filter(metric == "Dispersion")
metrics_corr <- all_metrics %>% filter(metric == "Correlation")

# Reference values
home_means <- metrics_home %>%
  group_by(league) %>%
  summarise(yintercept = mean(value), .groups = "drop")

# Ensure factor levels in subsets
home_means$league <- factor(home_means$league, levels = c("Bundesliga", "EPL", "La Liga"))

#   ____________________________________________________________________________
#   Color palette                                                           ####

type_colors <- c(
  "Single" = "gray30",
  "Home" = "#0072B2", # Blue
  "Away" = "#D55E00"
) # Orange

#   ____________________________________________________________________________
#   Plot A: Home Advantage                                                  ####

p_home <- ggplot(metrics_home, aes(x = season, y = value)) +
  geom_hline(
    data = home_means, aes(yintercept = yintercept),
    linetype = "dashed", color = "gray50", linewidth = 0.4
  ) +
  geom_line(color = "gray30", linewidth = 0.5) +
  geom_point(color = "gray30", size = 2.2) +
  facet_wrap(~league, nrow = 1) +
  scale_x_continuous(
    breaks = 2021:2025,
    labels = c("20/21", "21/22", "22/23", "23/24", "24/25")
  ) +
  scale_y_continuous(limits = c(-0.05, 0.55)) +
  labs(x = "Season", y = "Mean goal difference", tag = "A)") +
  theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(color = "gray70", linewidth = 0.4),
    panel.spacing = unit(0.8, "lines"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 30, vjust = 0.7, hjust = 0.5),
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 15)
  )

#   ____________________________________________________________________________
#   Plot B: Dispersion                                                      ####

p_disp <- ggplot(metrics_disp, aes(x = season, y = value, fill = type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6, alpha = 0.85
  ) +
  facet_wrap(~league, nrow = 1) +
  scale_fill_manual(
    values = type_colors,
    breaks = c("Home", "Away"),
    labels = c("Home", "Away"),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = 2021:2025,
    labels = c("20/21", "21/22", "22/23", "23/24", "24/25")
  ) +
  scale_y_continuous(limits = c(0, 1.5)) +
  labs(x = "Season", y = "Var/Mean ratio", tag = "B)") +
  theme_bw(base_size = 10) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(color = "gray70", linewidth = 0.4),
    panel.spacing = unit(0.8, "lines"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 30, vjust = 0.7, hjust = 0.5),
    legend.position = "top",
    legend.margin = margin(b = -5, t = -5),
    legend.text = element_text(size = 18),
    legend.key.size = unit(0.4, "cm"),
    plot.tag = element_text(face = "bold", size = 15)
  )

#   ____________________________________________________________________________
#   Plot D: Correlation Heatmap (renamed from C)                            ####

p_corr <- ggplot(metrics_corr, aes(x = season, y = league, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", value)),
    size = 5, fontface = "bold",
    color = ifelse(metrics_corr$value < -0.15, "white", "black")
  ) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#f7f7f7",
    high = "#4575b4",
    midpoint = 0,
    limits = c(-0.25, 0.10),
    name = expression(rho)
  ) +
  scale_x_continuous(
    breaks = 2021:2025,
    labels = c("20/21", "21/22", "22/23", "23/24", "24/25")
  ) +
  scale_y_discrete(limits = c("La Liga", "EPL", "Bundesliga")) +
  labs(x = "Season", y = "", tag = "D)") + # Changed to D)
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "gray70", linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 30, vjust = 0.7, hjust = 0.5),
    legend.position = "top",
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_text(size = 19),
    legend.margin = margin(b = -5, t = -5),
    legend.text = element_text(size = 15),
    plot.tag = element_text(face = "bold", size = 15)
  )

#   ____________________________________________________________________________
#   Inflation Parameter Analysis - Data Preparation                         ####

# Bundesliga preparation
season_boundaries_bund <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "05/10/2020",       "23/12/2020",        "01/01/2021",      "01/02/2021",
  2022,    "01/09/2021",       "23/12/2021",        "07/01/2022",      "31/01/2022",
  2023,    "01/09/2022",       "13/11/2022",        "20/01/2023",      "31/01/2023",
  2024,    "01/09/2023",       "22/12/2023",        "12/01/2024",      "01/02/2024",
  2025,    "30/08/2024",       "21/12/2024",        "10/01/2025",      "03/02/2025"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

bundesliga_model <- bundesliga %>%
  left_join(season_boundaries_bund, by = "season") %>%
  group_by(season) %>%
  arrange(season, Date) %>%
  mutate(
    match_id = row_number(),
    matchweek = ceiling(match_id / 9),
    season_period = case_when(
      Date <= summer_window_end ~ 1,
      Date > summer_window_end & Date < winter_break_start ~ 1,
      Date >= winter_break_end & Date <= winter_window_end ~ 2,
      Date > winter_window_end ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  arrange(season, Date) %>%
  mutate(
    periods = (season - min(season)) * 2 + season_period
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

colnames(bundesliga_model) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

# Premier League preparation
season_boundaries_prem <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "16/10/2020",       "01/01/2021",        "02/01/2021",      "01/02/2021",
  2022,    "31/08/2021",       "31/12/2021",        "01/01/2022",      "31/01/2022",
  2023,    "01/09/2022",       "14/11/2022",        "25/12/2022",      "31/01/2023",
  2024,    "01/09/2023",       "31/12/2023",        "01/01/2024",      "01/02/2024",
  2025,    "30/08/2024",       "02/01/2025",        "04/01/2025",      "03/02/2025"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

premier_model <- premier %>%
  left_join(season_boundaries_prem, by = "season") %>%
  group_by(season) %>%
  arrange(season, Date) %>%
  mutate(
    season_period = case_when(
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date <= summer_window_end ~ 1,
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date > summer_window_end & Date <= winter_break_start ~ 1,
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date > winter_break_start ~ 2,
      TRUE ~ NA
    )
  ) %>%
  ungroup() %>%
  arrange(season, Date) %>%
  mutate(
    periods = case_when(
      season == 2021 ~ season_period,
      season == 2022 ~ season_period + 2,
      season == 2023 ~ season_period + 4,
      season == 2024 ~ season_period + 6,
      season == 2025 ~ season_period + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

colnames(premier_model) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

# La Liga preparation
season_boundaries_liga <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "16/10/2020",       "04/01/2021",        "05/01/2021",      "01/02/2021",
  2022,    "31/08/2021",       "20/12/2021",        "30/12/2021",      "31/01/2022",
  2023,    "01/09/2022",       "14/11/2022",        "25/12/2022",      "31/01/2023", # World Cup season
  2024,    "01/09/2023",       "11/01/2024",        "01/01/2024",      "01/02/2024",
  2025,    "30/08/2024",       "23/12/2024",        "09/01/2025",      "03/02/2025"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

la_liga_model <- la_liga %>%
  left_join(season_boundaries_liga, by = "season") %>%
  group_by(season) %>%
  arrange(season, Date) %>%
  mutate(
    season_period = case_when(
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date <= summer_window_end ~ 1,
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date > summer_window_end & Date <= winter_break_start ~ 1,
      season %in% c(2021, 2022, 2023, 2024, 2025) & Date > winter_break_start ~ 2,
      TRUE ~ NA
    )
  ) %>%
  ungroup() %>%
  arrange(season, Date) %>%
  mutate(
    periods = case_when(
      season == 2021 ~ season_period,
      season == 2022 ~ season_period + 2,
      season == 2023 ~ season_period + 4,
      season == 2024 ~ season_period + 6,
      season == 2025 ~ season_period + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

colnames(la_liga_model) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

#   ____________________________________________________________________________
#   Inflation Parameter - Functions                                         ####

# Function to extract PP distribution for draws from a fitted model
extract_draw_pp <- function(object, data, season_label) {
  if (inherits(object, c("stanFoot", "CmdStanFit"))) {
    draws <- if (inherits(object, "stanFoot")) {
      object$fit$draws()
    } else {
      object$draws()
    }
    draws <- posterior::as_draws_rvars(draws)
    goal_diff_rep <- posterior::draws_of(draws[["diff_y_rep"]])
  } else if (inherits(object, "stanfit")) {
    sims <- rstan::extract(object)
    goal_diff_rep <- sims$diff_y_rep
  }

  if (median(goal_diff_rep) != round(median(goal_diff_rep), 0)) {
    goal_diff_rep <- round(goal_diff_rep, 0)
  }

  ngames <- ncol(goal_diff_rep)
  M <- nrow(goal_diff_rep)

  freq_draws_rep <- apply(goal_diff_rep, 1, function(row) {
    sum(row == 0) / ngames
  })

  y <- as.matrix(data[, c("home_goals", "away_goals")])
  goal_diff_obs <- y[, 1] - y[, 2]
  freq_draws_obs <- sum(goal_diff_obs == 0) / ngames

  p_value <- sum(freq_draws_rep >= freq_draws_obs) / M

  data.frame(
    season = season_label,
    freq_draws = freq_draws_rep,
    obs_freq = freq_draws_obs,
    p_value = p_value
  )
}

# Run seasonal PP analysis for a league
run_seasonal_pp_analysis <- function(full_data, games_per_season = 380) {
  n_seasons <- nrow(full_data) / games_per_season
  results_list <- list()

  for (s in 1:n_seasons) {
    cat("Fitting season", s, "...\n")

    idx_start <- (s - 1) * games_per_season + 1
    idx_end <- s * games_per_season
    season_data <- full_data[idx_start:idx_end, ]

    fit <- stan_foot(
      data = season_data,
      model = "double_pois",
      dynamic_type = "seasonal",
      dynamic_weight = TRUE,
      dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
      home_effect = TRUE,
      iter_sampling = 1000, chains = 4,
      parallel_chains = 4, init = 0,
      method = "MCMC", seed = 2323332, show_exceptions = FALSE, show_messages = FALSE
    )

    results_list[[s]] <- extract_draw_pp(fit, season_data, paste0("Season ", s))
  }

  bind_rows(results_list)
}

# Create pp_foot style plot for draws (goal diff = 0) - one league
create_league_pp_plot <- function(pp_results, league_name, show_title = TRUE, show_legend = FALSE) {
  # Rename seasons to match other plots
  pp_results <- pp_results %>%
    mutate(
      season = factor(season,
        levels = c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5"),
        labels = c("20/21", "21/22", "22/23", "23/24", "24/25")
      ),
      season_num = as.numeric(season)
    )

  # Get observed frequencies and mean simulated per season
  summary_stats <- pp_results %>%
    group_by(season, season_num) %>%
    summarise(
      obs_freq = first(obs_freq),
      p_value = first(p_value),
      mean_sim = mean(freq_draws),
      .groups = "drop"
    )

  p <- ggplot(pp_results, aes(x = season_num, y = freq_draws)) +
    # Simulated points (jittered)
    geom_point(aes(color = "Simulated"),
      position = position_jitter(width = 0.25, height = 0),
      alpha = 0.15, size = 0.8
    ) +
    # Observed segments (horizontal bars)
    geom_segment(
      data = summary_stats,
      aes(
        x = season_num - 0.4, xend = season_num + 0.4,
        y = obs_freq, yend = obs_freq, color = "Observed"
      ),
      linewidth = 1.5
    ) +
    # PPP annotation (top)
    geom_text(
      data = summary_stats,
      aes(
        x = season_num, y = 0.425,
        label = paste0("PPP = ", sprintf("%.2f", p_value))
      ),
      size = 3.0, color = "gray30"
    ) +
    # # Mean simulated annotation (middle)
    # geom_text(data = summary_stats,
    #           aes(x = season_num, y = 0.425,
    #               label = paste0("\u03BC[sim] = ", sprintf("%.2f", mean_sim))),
    #           size = 3.3, color = "#FFA500", parse = FALSE) +
    # # Observed annotation (bottom)
    # geom_text(data = summary_stats,
    #           aes(x = season_num, y = 0.405,
    #               label = paste0("obs = ", sprintf("%.2f", obs_freq))),
    #           size = 3.3, color = "#1E90FF") +
    scale_color_manual(
      name = "",
      values = c("Observed" = "#1E90FF", "Simulated" = "#FFA500"),
      guide = guide_legend(override.aes = list(
        linetype = c("solid", "blank"),
        shape = c(NA, 16),
        size = c(1.5, 2),
        alpha = c(1, 0.6)
      ))
    ) +
    scale_x_continuous(
      breaks = 1:5,
      labels = c("20/21", "21/22", "22/23", "23/24", "24/25")
    ) +
    scale_y_continuous(
      limits = c(0.15, 0.48),
      breaks = c(0.20, 0.25, 0.30, 0.35, 0.40)
    ) +
    labs(
      x = "Season",
      y = "Draw frequency",
      title = if (show_title) league_name else NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = if (show_legend) "top" else "none",
      legend.margin = margin(b = -5, t = -5),
      legend.key.size = unit(0.4, "cm"),
      strip.text = element_text(size = 18),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "gray70", linewidth = 0.4),
      panel.spacing = unit(0.8, "lines"),
      axis.ticks = element_line(linewidth = 0.3),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 22),
      axis.title.x = element_text(size = 22),
      axis.text.x = element_text(size = 15, angle = 30, vjust = 0.7, hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 15)
    )

  return(p)
}

#   ____________________________________________________________________________
#   Run PP Analysis for All Leagues                                         ####

cat("Running PP analysis for Bundesliga...\n")
pp_bundesliga <- run_seasonal_pp_analysis(bundesliga_model, games_per_season = 306)

cat("Running PP analysis for EPL...\n")
pp_epl <- run_seasonal_pp_analysis(premier_model, games_per_season = 380)

cat("Running PP analysis for La Liga...\n")
pp_laliga <- run_seasonal_pp_analysis(la_liga_model, games_per_season = 380)

#   ____________________________________________________________________________
#   Plot C: Inflation (Draw PP) - Three Panels                              ####

# Add tag only to the first plot in the row, legend on the middle plot
p_inflation_bund <- create_league_pp_plot(pp_bundesliga, "Bundesliga", show_legend = FALSE) +
  labs(tag = "C)") +
  theme(plot.tag = element_text(face = "bold", size = 12))

p_inflation_epl <- create_league_pp_plot(pp_epl, "EPL", show_legend = TRUE)
p_inflation_liga <- create_league_pp_plot(pp_laliga, "La Liga", show_legend = FALSE)

# Combine the three inflation plots horizontally
p_inflation_row <- p_inflation_bund + p_inflation_epl + p_inflation_liga +
  plot_layout(nrow = 1)

#   ____________________________________________________________________________
#   Combine with patchwork                                                  ####

p_combined <- p_home / p_disp / p_inflation_row / p_corr +
  plot_layout(heights = c(0.8, 0.8, 0.8, 0.8))

p_combined

# Save the plot
ggsave("football_metrics_combined.pdf",
  path = "Plots",
  plot = p_combined,
  width = 20, height = 14, device = "pdf", dpi = 500, useDingbats = FALSE
)







#   ============================================================================
#   Publication-quality figure for JRSS-C
#   ============================================================================
#
#   This script assumes all data loading, metric calculation, model fitting,
#   and PP analysis have already been run (see original script). It only
#   replaces the plotting section.
#
#   Key changes from the original:
#     - Consistent, modest font sizing appropriate for a two-column journal
#     - Panel labels use lowercase (a)–(d) per JRSS-C convention
#     - Grayscale-friendly palette with distinct shapes for print
#     - Tightened spacing, no redundant grid lines
#     - More informative axis labels
#     - Uniform theme across all panels
#   ============================================================================

library(ggplot2)
library(patchwork)

#   ____________________________________________________________________________
#   Global theme & palette                                                  ####

# Base theme for all panels
theme_jrssc <- function(base_size = 10) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # Text — matched to ability_plot font sizes
      plot.tag = element_text(face = "bold", size = 15, hjust = 0),
      strip.text = element_text(size = 18, margin = margin(t = 4, b = 4)),
      strip.background = element_rect(
        fill = "grey95", colour = "grey70",
        linewidth = 0.5
      ),
      # Axes
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 15),
      axis.text.x = element_text(size = 11, angle = 30, hjust = 0.5),
      axis.ticks = element_line(linewidth = 0.3, colour = "grey50"),
      # Panel
      panel.border = element_rect(colour = "grey50", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.25),
      panel.spacing = unit(0.8, "lines"),
      # Legend
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.size = unit(0.4, "cm"),
      legend.background = element_blank(),
      # Margins
      plot.margin = margin(t = 4, r = 6, b = 2, l = 4)
    )
}

# Season labels (reused everywhere)
season_labels <- c("20/21", "21/22", "22/23", "23/24", "24/25")

# Palette for Home / Away (distinguishable in greyscale)
palette_ha <- c("Home" = "#FFA500", "Away" = "#D55E00")

#   ____________________________________________________________________________
#   (a) Home advantage                                                      ####

p_home <- ggplot(metrics_home, aes(x = season, y = value)) +
  geom_hline(
    data = home_means, aes(yintercept = yintercept),
    linetype = "dashed", colour = "grey55", linewidth = 0.35
  ) +
  geom_line(colour = "grey30", linewidth = 0.45) +
  geom_point(colour = "grey20", size = 1.6, shape = 16) +
  facet_wrap(~league, nrow = 1) +
  scale_x_continuous(breaks = 2021:2025, labels = season_labels) +
  scale_y_continuous(
    limits = c(-0.05, 0.55),
    breaks = seq(0, 0.5, 0.1)
  ) +
  labs(x = NULL, y = "Mean goal difference", tag = "A)") +
  theme_jrssc() +
  theme(
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5)
  )

#   ____________________________________________________________________________
#   (b) Dispersion                                                          ####

p_disp <- ggplot(
  metrics_disp %>% mutate(type = factor(type, levels = c("Home", "Away"))),
  aes(x = season, y = value, fill = type)
) +
  geom_hline(
    yintercept = 1, linetype = "dashed", colour = "grey55",
    linewidth = 0.35
  ) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.55, colour = "grey30", linewidth = 0.2
  ) +
  facet_wrap(~league, nrow = 1) +
  scale_fill_manual(
    values = palette_ha,
    breaks = c("Home", "Away"),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2021:2025, labels = season_labels) +
  scale_y_continuous(
    limits = c(0, 1.55),
    breaks = seq(0, 1.5, 0.25)
  ) +
  labs(x = NULL, y = "Variance to mean ratio", tag = "B)") +
  theme_jrssc() +
  theme(
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
    legend.position = "top",
    legend.margin = margin(t = -2, b = -4)
  )

#   ____________________________________________________________________________
#   (c) Posterior predictive check — draw frequency                         ####

# Combine all PP results with a league column
pp_combined <- bind_rows(
  pp_bundesliga %>% mutate(league = "Bundesliga"),
  pp_epl %>% mutate(league = "EPL"),
  pp_laliga %>% mutate(league = "La Liga")
) %>%
  mutate(
    league = factor(league, levels = c("Bundesliga", "EPL", "La Liga")),
    season = factor(season,
      levels = paste0("Season ", 1:5),
      labels = season_labels
    ),
    season_num = as.numeric(season)
  )

pp_summary <- pp_combined %>%
  group_by(league, season, season_num) %>%
  summarise(
    obs_freq = first(obs_freq),
    p_value  = first(p_value),
    mean_sim = mean(freq_draws),
    .groups  = "drop"
  )

p_inflation_row <- ggplot(pp_combined, aes(x = season_num, y = freq_draws)) +
  # Simulated (posterior predictive) cloud
  geom_point(aes(colour = "Simulated"),
    position = position_jitter(width = 0.22, height = 0),
    alpha = 0.08, size = 0.5, shape = 16
  ) +
  # Observed horizontal bar
  geom_segment(
    data = pp_summary,
    aes(
      x = season_num - 0.35, xend = season_num + 0.35,
      y = obs_freq, yend = obs_freq, colour = "Observed"
    ),
    linewidth = 1.0
  ) +
  # PPP values
  geom_text(
    data = pp_summary,
    aes(
      x = season_num, y = 0.44,
      label = sprintf("%.2f", p_value)
    ),
    size = 4, colour = "grey30"
  ) +
  # Label row above
  geom_text(
    data = pp_summary %>% filter(season_num == 3),
    aes(x = season_num, y = 0.47),
    label = "PPP-values", size = 4, colour = "grey40"
  ) +
  facet_wrap(~league, nrow = 1) +
  scale_colour_manual(
    name = NULL,
    values = c("Observed" = "#083e9a", "Simulated" = "#88beff"),
    guide = guide_legend(override.aes = list(
      linetype = c("solid", "blank"),
      shape    = c(NA, 16),
      size     = c(1.0, 1.8),
      alpha    = c(1, 0.5)
    ))
  ) +
  scale_x_continuous(breaks = 1:5, labels = season_labels) +
  scale_y_continuous(
    limits = c(0.14, 0.48),
    breaks = seq(0.15, 0.45, 0.05)
  ) +
  labs(x = "Season", y = "Draw frequency", tag = "C)") +
  theme_jrssc() +
  theme(
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
    legend.position = "top",
    legend.margin = margin(t = -2, b = -4)
  )

#   ____________________________________________________________________________
#   (d) Correlation heatmap                                                 ####

p_corr <- ggplot(metrics_corr, aes(x = season, y = league, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(
    aes(
      label = sprintf("%.2f", value),
      colour = abs(value) > 0.15
    ),
    size = 7, fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey15")) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#F7F7F7",
    high     = "#4575b4",
    midpoint = 0,
    limits   = c(-0.25, 0.10),
    breaks   = seq(-0.2, 0.1, 0.1)
  ) +
  scale_x_continuous(breaks = 2021:2025, labels = season_labels) +
  scale_y_discrete(limits = c("La Liga", "EPL", "Bundesliga")) +
  labs(x = "Season", y = NULL, tag = "D)") +
  theme_jrssc() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
    legend.position = "top",
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = -2, b = -4)
  )

#   ____________________________________________________________________________
#   Assemble final figure                                                   ####

p_final <- p_home / p_disp / p_inflation_row / p_corr +
  plot_layout(heights = c(1, 1, 1, 0.7))


# Save the plot
ggsave("football_metrics_combined.pdf",
  path = "Plots",
  plot = p_final,
  width = 20, height = 15, device = "pdf", dpi = 500, useDingbats = FALSE
)
