#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

#   ____________________________________________________________________________
#   Load                                                                    ####

load("~/Desktop/Work/Projects/HBGM/comparison_premier.RData")
load("~/Desktop/Work/Projects/HBGM/comparison_ligue_1.RData")
load("~/Desktop/Work/Projects/HBGM/comparison_liga.RData")
load("~/Desktop/Work/Projects/HBGM/comparison_bundes.RData")

# Merging data

comparison_bundes$metrics$league <- "Bundesliga"
comparison_liga$metrics$league <- "La Liga"
comparison_ligue$metrics$league <- "Ligue 1"
comparison_premier$metrics$league <- "Premier League"

bundesliga <-  comparison_bundes$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )
liga <- comparison_liga$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  ) 
ligue <- comparison_ligue$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  ) 
premier <- comparison_premier$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  ) 



combined <- bind_rows(bundesliga, liga, ligue, premier) %>%
  mutate(
    # 1) detect suffix and assign label
    model_type = case_when(
      str_detect(Model, "_comm$")  ~ "Weighted Dynamic",
      str_detect(Model, "_egidi$") ~ "Egidi et al. (2018)",
      str_detect(Model, "_owen$")  ~ "Owen (2011)",
      TRUE                         ~ "Dynamic"
    ),
    # 2) strip off ANY of the three suffixes
    base = str_remove(Model, "_(comm|egidi|owen)$"),
    # 3) map to your human‐readable goal names
    goal_model = case_when(
      base == "biv_pois"        ~ "Bivariate Poisson",
      base == "diag_biv_pois"   ~ "Diagonal Bivariate Poisson",
      base == "double_pois"     ~ "Double Poisson",
      base == "neg_bin"         ~ "Negative Binomial",
      base == "skellam"         ~ "Skellam",
      base == "zero_skellam"    ~ "Zero-Inflated Skellam",
      TRUE                       ~ NA_character_
    )
  ) %>%
  select(-base)
head(combined)

# Transform dataset in long

df_long <- combined %>%
  pivot_longer(
    cols      = c(ACP, pseudoR2, RPS, brier),
    names_to  = "measure",
    values_to = "score"
  ) %>%
  # reorder and relabel the measure factor
  mutate(
    measure = factor(
      measure,
      levels = c("ACP", "pseudoR2", "RPS", "brier"),
      labels = c("ACP", "Pseudo R^2", "RPS", "Brier Score")
    ),
    goal_model = factor(
      goal_model,
      levels = c(
        "Bivariate Poisson",
        "Diagonal Bivariate Poisson",
        "Double Poisson",
        "Negative Binomial",
        "Skellam",
        "Zero-Inflated Skellam"
      ),
      labels = c(
        "BP",
        "DIBP",
        "DP",
        "NB",
        "SM",
        "ZISM"
      )
    )
  )

#   ____________________________________________________________________________
#   Plots                                                                   ####


plot_comp <- ggplot(
  df_long,
  aes(
    x = goal_model,
    y = score,
    group = model_type,
    color = model_type,
    shape = model_type
  )
) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(measure ~ league, scales = "free_y") +
  scale_shape_manual(values = c(
    "Weighted Dynamic" = 16,
    "Owen (2011)" = 18, 
    "Egidi et al. (2018)" = 15
  )) +
  labs(
    x     = "Model",
    y     = NULL,
    color = "Model type",
    shape = "Model type"
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
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey95"),
  )


ggsave(
  filename = "plot_comp.pdf", path = "Plots",
  plot = plot_comp,
  width = 17, height = 10, device = "pdf", dpi = 500, useDingbats = FALSE
)




# assuming df_long already built as in your example

df_by_league <- df_long %>% split(.$league)

plots_by_league <- imap(df_by_league, ~ {
  ggplot(.x, aes(x = goal_model, y = score,
                 group = model_type,
                 color = model_type,
                 shape = model_type)) +
    geom_line() +
    geom_point(size = 2) +
    facet_grid(measure ~ ., scales = "free_y") +
    scale_shape_manual(values = c(
      "Weighted Dynamic"    = 16,
      "Owen (2011)"         = 18,
      "Egidi et al. (2018)" = 15
    )) +
    labs(
      title = .y,            # league name as the title
      x     = "Model",
      y     = NULL,
      color = "Model type",
      shape = "Model type"
    ) +
    theme_bw() +
    theme(
      strip.placement   = "outside",
      strip.text.x = element_text(size = 18),
      strip.text.y = element_text(size = 18),
      legend.position = "top",
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      axis.title.y = element_text(size = 22),
      axis.title.x = element_text(size = 22),
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      plot.title        = element_text(size = 18, face = "bold", hjust = 0.5),
      strip.background  = element_rect(fill = "grey95")
    )
})

plots_by_league[["Premier League"]]

purrr::iwalk(plots_by_league, ~ {
  ggsave(filename = paste0("plot_", gsub(" ", "_", .y), ".pdf"),
         path = "Plots",
         plot     = .x,
         width = 17, height = 10, device = "pdf", dpi = 500, useDingbats = FALSE)
})
