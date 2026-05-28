#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(footBayes)
library(knitr)
library(kableExtra)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Bundesliga/Data_Bundesliga.R")
source("Scripts/Premier_League/Data_Premier.R")
source("Scripts/La_Liga/Data_Liga.R")


#   ____________________________________________________________________________
#   Ability medians for selected teams function                             ####

get_ability_df <- function(stan_obj, teams, all_teams) {
  draws <- stan_obj$fit$draws() |> posterior::as_draws_rvars()
  att_arr <- posterior::draws_of(draws[["att"]])
  def_arr <- posterior::draws_of(draws[["def"]])

  idx <- match(teams, all_teams)
  if (any(is.na(idx))) {
    stop("Teams not found: ", paste(teams[is.na(idx)], collapse = ", "))
  }

  T_periods <- dim(att_arr)[2]

  summarize_array <- function(arr) {
    list(
      med = apply(arr, c(2, 3), median)[, idx, drop = FALSE],
      lo  = apply(arr, c(2, 3), quantile, probs = 0.025)[, idx, drop = FALSE],
      hi  = apply(arr, c(2, 3), quantile, probs = 0.975)[, idx, drop = FALSE]
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
        effect = effect
      )
  }

  bind_rows(make_df(a, "attack"), make_df(d, "defense"))
}


#   ____________________________________________________________________________
#   Sensitivity grid                                                        ####

grid <- expand.grid(
  slab_sd = c(3, 4, 5),
  spike_mu = c(9, 10, 15, 20, 50, 100),
  stringsAsFactors = FALSE
) |>
  mutate(
    scenario = paste0("slab_sd=", slab_sd, "_spike_mu=", spike_mu),
    is_baseline = (slab_sd == 3 & spike_mu == 9)
  )

cat("Sensitivity grid (", nrow(grid), " scenarios per league):\n")
print(grid)


#   ____________________________________________________________________________
#   League configuration                                                    ####

league_config <- list(
  Bundesliga = list(
    data      = bundesliga,
    model     = "skellam",
    teams     = c("Bayern Munich", "Hoffenheim"),
    seed_base = 2323332
  ),
  EPL = list(
    data      = premier,
    model     = "double_pois",
    teams     = c("Man City", "Man United"),
    seed_base = 2323332
  ),
  La_Liga = list(
    data      = la_liga,
    model     = "dixon_coles",
    teams     = c("Real Madrid", "Girona"),
    seed_base = 2323332
  )
)


#   ____________________________________________________________________________
#   Run all fits                                                            ####

results <- list()

for (lg_name in names(league_config)) {
  cfg <- league_config[[lg_name]]
  all_teams <- unique(c(cfg$data$home_team, cfg$data$away_team))

  for (i in seq_len(nrow(grid))) {
    sc <- grid[i, ]
    label <- paste0(lg_name, " | ", sc$scenario)
    cat("\n=== Fitting:", label, "===\n")

    fit <- stan_foot(
      data = cfg$data,
      model = cfg$model,
      dynamic_type = "seasonal",
      dynamic_weight = TRUE,
      dynamic_par = list(
        slab  = normal(0, sc$slab_sd),
        spike = normal(sc$spike_mu, 1.5)
      ),
      home_effect = TRUE,
      iter_sampling = 2000,
      chains = 4,
      parallel_chains = 4,
      init = 0,
      method = "MCMC",
      seed = cfg$seed_base
    )

    df <- get_ability_df(fit, cfg$teams, all_teams) |>
      mutate(
        league      = lg_name,
        slab_sd     = sc$slab_sd,
        spike_mu    = sc$spike_mu,
        scenario    = sc$scenario,
        is_baseline = sc$is_baseline
      )

    results[[length(results) + 1]] <- df

    cat("  Done:", label, "\n")
  }
}


#   ____________________________________________________________________________
#   Combine results                                                         ####

df_all <- bind_rows(results) |>
  mutate(
    team = fct_relevel(
      team,
      "Bayern Munich", "Hoffenheim",
      "Man City", "Man United",
      "Real Madrid", "Girona"
    ),
    league = case_when(
      league == "La_Liga" ~ "La Liga",
      TRUE ~ league
    )
  )

save(df_all, file = "RData/sensitivity_results.RData")


#   ____________________________________________________________________________
#   Period labels                                                           ####

period_labels <- c(
  "1"  = "20/21-H1", "2"  = "20/21-H2",
  "3"  = "21/22-H1", "4"  = "21/22-H2",
  "5"  = "22/23-H1", "6"  = "22/23-H2",
  "7"  = "23/24-H1", "8"  = "23/24-H2",
  "9"  = "24/25-H1", "10" = "24/25-H2"
)



#   ____________________________________________________________________________
#   Sensitivity Boxplot                                                     ####

df_plot <- df_all |>
  mutate(
    team = fct_relevel(
      team,
      "Bayern Munich", "Hoffenheim",
      "Man City", "Man United",
      "Real Madrid", "Girona"
    ),
    effect = factor(effect,
      levels = c("attack", "defense"),
      labels = c("Attack", "Defense")
    ),
    is_baseline = (slab_sd == 3 & spike_mu == 9),
    period_label = factor(period_labels[as.character(period)],
      levels = period_labels
    )
  )

# Separate baseline and alternatives
df_alt <- df_plot |> filter(!is_baseline)
df_base <- df_plot |> filter(is_baseline)

#   ____________________________________________________________________________
#   Plot                                                                    ####

sensitivity_boxplot <- ggplot() +
  geom_boxplot(
    data = df_plot,
    aes(x = period_label, y = mid, fill = effect),
    outlier.shape = NA,
    linewidth = 0.4,
    width = 0.6,
    alpha = 0.4,
    coef = 1.5
  ) +
  geom_jitter(
    data = df_alt,
    aes(x = period_label, y = mid),
    colour = "grey60",
    size = 1.5,
    alpha = 0.8,
    width = 0.15,
    height = 0
  ) +
  geom_point(
    data = df_base,
    aes(x = period_label, y = mid, colour = effect),
    size = 2.7,
    shape = 17
  ) +
  geom_hline(
    yintercept = 0, linetype = "dotdash", colour = "grey50",
    linewidth = 0.5
  ) +
  facet_grid(
    team ~ effect,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(Attack = "#f5d1d1", Defense = "#d7e5f1"),
    guide  = "none"
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(Attack = "#cd1719", Defense = "#377EB8"),
    labels = c(
      Attack  = "Attack (baseline)",
      Defense = "Defense (baseline)"
    )
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 4))
  ) +
  labs(
    x = "Season / Period",
    y = "Ability values"
  ) +
  theme_bw(base_size = 13) +
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

sensitivity_boxplot

# Save
ggsave(
  filename = "sensitivity_boxplot.pdf",
  path = "Plots",
  plot = sensitivity_boxplot,
  width = 20, height = 18,
  device = "pdf", dpi = 500, useDingbats = FALSE
)
