#   ____________________________________________________________________________
#   Libraries                                                               ####

library(bayesplot)
library(posterior)
library(ggplot2)
library(dplyr)
library(tidyr)
library(footBayes)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Bundesliga/Data_Bundesliga.R")
source("Scripts/Premier_League/Data_Premier.R")
source("Scripts/La_Liga/Data_Liga.R")

#   ____________________________________________________________________________
#   Refit best weighted dynamic model per league (full data, no predict)   ####

### Bundesliga — Skellam
skellam_comm_diag <- stan_foot(
  data = bundesliga,
  model = "skellam",
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(slab = normal(0, 3), spike = normal(9, 1.5)),
  home_effect = TRUE,
  iter_sampling = 2000, chains = 4,
  parallel_chains = 4,
  method = "MCMC", seed = 2333332
)

### EPL — Double Poisson
double_pois_comm_diag <- stan_foot(
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

### La Liga — Dixon-Coles
dixon_coles_comm_diag <- stan_foot(
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

save(skellam_comm_diag, double_pois_comm_diag, dixon_coles_comm_diag,
  file = "RData/diag_models.RData"
)

#   ____________________________________________________________________________
#   Team lists + period labels                                             ####

all_teams_bundes <- unique(c(bundesliga$home_team, bundesliga$away_team))
all_teams_epl <- unique(c(premier$home_team, premier$away_team))
all_teams_liga <- unique(c(la_liga$home_team, la_liga$away_team))

period_labels <- c(
  "1"  = "20/21-H1", "2"  = "20/21-H2",
  "3"  = "21/22-H1", "4"  = "21/22-H2",
  "5"  = "22/23-H1", "6"  = "22/23-H2",
  "7"  = "23/24-H1", "8"  = "23/24-H2",
  "9"  = "24/25-H1", "10" = "24/25-H2"
)

#   ____________________________________________________________________________
#   Helper: extract att + def draws and convert to long data frame         ####

extract_ability_long <- function(stan_obj, teams, all_teams, n_periods = 10) {
  draws <- stan_obj$fit$draws(format = "draws_df")
  vars <- names(draws)

  idx <- match(teams, all_teams)
  if (any(is.na(idx))) {
    stop("Team(s) not found: ", paste(teams[is.na(idx)], collapse = ", "))
  }

  # Build variable names [time, team] — matches Stan matrix[ntimes, nteams]
  att_vars <- as.vector(outer(1:n_periods, idx, function(t, i) paste0("att[", t, ",", i, "]")))
  def_vars <- as.vector(outer(1:n_periods, idx, function(t, i) paste0("def[", t, ",", i, "]")))
  keep <- intersect(c(att_vars, def_vars), vars)

  # Fallback: [team, time] ordering
  if (length(keep) == 0) {
    att_vars <- as.vector(outer(idx, 1:n_periods, function(i, t) paste0("att[", i, ",", t, "]")))
    def_vars <- as.vector(outer(idx, 1:n_periods, function(i, t) paste0("def[", i, ",", t, "]")))
    keep <- intersect(c(att_vars, def_vars), vars)
  }

  if (length(keep) == 0) stop("No att/def variables found.")

  # Keep only relevant columns + chain/iteration metadata
  draws_sub <- draws[, c(".chain", ".iteration", ".draw", keep)]

  # Pivot to long format
  draws_long <- draws_sub |>
    tidyr::pivot_longer(
      cols      = all_of(keep),
      names_to  = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      # Parse effect, period, team_idx from variable name
      effect = sub("\\[.*", "", variable),
      period = as.integer(sub(".*\\[(\\d+),(\\d+)\\]", "\\1", variable)),
      team_idx = as.integer(sub(".*\\[(\\d+),(\\d+)\\]", "\\2", variable)),
      team = teams[match(team_idx, idx)],
      period_lbl = factor(period_labels[as.character(period)],
        levels = unname(period_labels)
      )
    )

  draws_long
}

#   ____________________________________________________________________________
#   Helper: ACF plot with triple facet                                     ####
#   columns = effect (att / def)
#   rows    = team x period                                                ####
bb_colors <- color_scheme_get("brightblue")
make_acf_plot <- function(draws_long, label) {
  # Compute ACF per (chain, team, effect, period)
  max_lag <- 40

  acf_df <- draws_long |>
    dplyr::group_by(.chain, team, effect, period_lbl) |>
    dplyr::arrange(.iteration, .by_group = TRUE) |>
    dplyr::summarise(
      acf_vals = list(as.numeric(acf(value, lag.max = max_lag, plot = FALSE)$acf)),
      .groups  = "drop"
    ) |>
    tidyr::unnest_longer(acf_vals, indices_to = "lag") |>
    dplyr::mutate(
      lag   = lag - 1L, # acf() returns lag 0 first
      chain = factor(.chain, labels = paste("Chain", sort(unique(.chain))))
    ) |>
    dplyr::filter(lag > 0) # drop lag 0 (always 1)

  ggplot(acf_df, aes(x = lag, y = acf_vals, color = chain)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
    geom_hline(
      yintercept = 1.96 / sqrt(2000), linetype = "dashed",
      color = "grey30", linewidth = 0.35
    ) +
    geom_hline(
      yintercept = -1.96 / sqrt(2000), linetype = "dashed",
      color = "grey30", linewidth = 0.35
    ) +
    geom_segment(aes(xend = lag, yend = 0),
      linewidth = 0.4,
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(
      rows = vars(team, period_lbl),
      cols = vars(effect),
      labeller = labeller(
        effect = c(att = "Attack", def = "Defense")
      )
    ) +
    scale_color_brewer(palette = unlist(bb_colors), name = NULL) +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40)) +
    labs(
      title = paste(label, "\u2014 Autocorrelation (up to lag 40)"),
      x     = "Lag",
      y     = "Autocorrelation"
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title      = element_text(size = 11, face = "bold"),
      strip.text.x    = element_text(size = 8, face = "bold"),
      strip.text.y    = element_text(size = 7),
      legend.position = "top",
      legend.text     = element_text(size = 8),
      axis.text       = element_text(size = 7),
      panel.spacing   = unit(0.4, "lines")
    )
}

#   ____________________________________________________________________________
#   Helper: Trace plot with triple facet                                   ####
chain_colors <- c(
  "#cce5ff", "#99cbff",
  "#198bff", "#004c99"
)
make_trace_plot <- function(draws_long, label) {
  ggplot(
    draws_long,
    aes(x = .iteration, y = value, color = factor(.chain))
  ) +
    geom_line(alpha = 0.6, linewidth = 0.3) +
    facet_grid(
      rows = vars(team, period_lbl),
      cols = vars(effect),
      scales = "free_y",
      labeller = labeller(
        effect = c(att = "Attack", def = "Defense")
      )
    ) +
    scale_color_manual(
      values = unname(chain_colors), name = NULL,
      labels = paste("Chain", 1:4)
    ) +
    labs(
      x     = "Iteration",
      y     = "Value"
    ) +
    theme_bw(base_size = 9) +
    theme(
      strip.placement = "outside",
      strip.text.x = element_text(size = 9),
      strip.text.y = element_text(size = 7),
      legend.position = "top",
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      panel.spacing = unit(0.5, "lines")
    )
}

#   ____________________________________________________________________________
#   Helper: save both plots                                                ####

save_diag_plots <- function(stan_obj, teams, all_teams, label,
                            out_dir = "Plots/Diagnostics") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  draws_long <- extract_ability_long(stan_obj, teams, all_teams)
  p_trace <- make_trace_plot(draws_long, label)

  n_rows <- length(teams) * 10
  h <- 15

  ggsave(file.path(out_dir, paste0("trace_", label, ".pdf")),
    p_trace,
    width = 10, height = h, device = "pdf", dpi = 500,
    useDingbats = FALSE
  )

  message("Saved trace plot for: ", label)
}

#   ____________________________________________________________________________
#   Run for all three leagues — same teams as Figure 2                     ####

save_diag_plots(
  stan_obj  = skellam_comm_diag,
  teams     = c("Bayern Munich", "Hoffenheim"),
  all_teams = all_teams_bundes,
  label     = "bundesliga"
)

save_diag_plots(
  stan_obj  = double_pois_comm_diag,
  teams     = c("Man City", "Man United"),
  all_teams = all_teams_epl,
  label     = "epl"
)

save_diag_plots(
  stan_obj  = dixon_coles_comm_diag,
  teams     = c("Real Madrid", "Girona"),
  all_teams = all_teams_liga,
  label     = "liga"
)

