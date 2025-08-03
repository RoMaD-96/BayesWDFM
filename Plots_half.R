#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

#   ____________________________________________________________________________
#   Load                                                                    ####

load("~/Desktop/Work/Projects/HBGM/comparison_premier_half_30.RData")
load("~/Desktop/Work/Projects/HBGM/comparison_liga_half_30.RData")
load("~/Desktop/Work/Projects/HBGM/comparison_bundes_half_27.RData")

# Merging data

comparison_bundes_half$metrics$league <- "Bundesliga"
comparison_liga_half$metrics$league <- "La Liga"
comparison_premier_half$metrics$league <- "Premier League"

bundesliga <-  comparison_bundes_half$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )
liga <- comparison_liga_half$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  ) 

premier <- comparison_premier_half$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  ) 



combined <- bind_rows(bundesliga, liga, premier) %>%
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
  filename = "plot_comp_half.pdf", path = "Plots",
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
  ggsave(filename = paste0("plot_half_", gsub(" ", "_", .y), ".pdf"),
         path = "Plots",
         plot     = .x,
         width = 17, height = 10, device = "pdf", dpi = 500, useDingbats = FALSE)
})


#   ____________________________________________________________________________
#   Computational Time Plot                                                 ####

load("~/Desktop/Work/Projects/HBGM/RData/Bundesliga/time_results_bundes_half.RData")    
load("~/Desktop/Work/Projects/HBGM/RData/La_Liga/time_results_liga_half.RData") 
load("~/Desktop/Work/Projects/HBGM/RData/Premier_League/time_results_premier_half.RData")

df_bundes <- time_results_bundes_half    %>% mutate(league = "Bundesliga")
df_liga   <- time_results_liga_half     %>% mutate(league = "La Liga")
df_epl    <- time_results_premier_half  %>% mutate(league = "EPL")

df_all <- bind_rows(df_bundes, df_liga, df_epl)

df_all <- df_all %>%
  mutate(
    model_type = case_when(
      str_detect(model, "_comm$") ~ "Weighted Dynamic",
      str_detect(model, "_owen$") ~ "Owen (2011)",
      TRUE                        ~ "Egidi et al. (2018)"
    ),
    # force the factor levels in the desired order
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)")
    ),
    goal_model = case_when(
      str_detect(model, "^biv_pois")      ~ "Bivariate Poisson",
      str_detect(model, "^diag_biv_pois") ~ "Diag. Infl. Biv. Poisson",
      str_detect(model, "^double_pois")    ~ "Double Poisson",
      str_detect(model, "^neg_bin")        ~ "Negative Binomial",
      str_detect(model, "^skellam")        ~ "Skellam",
      str_detect(model, "^zero_skellam")   ~ "Zero-inflated Skellam",
      TRUE                                 ~ NA_character_
    )
  )

my_cols <- c(
  "Weighted Dynamic"    = "#E69F00",
  "Owen (2011)"         = "#440154",
  "Egidi et al. (2018)" = "#29AF7F"
)

comp_time_half <- ggplot(df_all, aes(x = model_type, y = elapsed, fill = model_type)) +
  geom_boxplot(outlier.size = 1) +
  scale_fill_manual(values = my_cols, name = "") +
  facet_grid(goal_model ~ league, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position  = "top",
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 14),
    strip.text       = element_text(size = 10),
    strip.background = element_rect(fill = "grey95"),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12)
  ) +
  labs(
    x     = NULL,
    y     = "Elapsed Time (seconds)")


ggsave("comp_time_half.pdf",
       plot = comp_time_half, path = "Plots",
       width = 12, height = 10, device = "pdf", dpi = 500, useDingbats = FALSE
)




df_pct_gain <- df_all %>%
  group_by(league, goal_model) %>%
  summarise(
    mean_wd    = mean(elapsed[model_type == "Weighted Dynamic"],    na.rm = TRUE),
    mean_owen  = mean(elapsed[model_type == "Owen (2011)"],        na.rm = TRUE),
    mean_egidi = mean(elapsed[model_type == "Egidi et al. (2018)"], na.rm = TRUE),
    # % gain relative to each original method:
    pct_gain_vs_owen  = (mean_owen  - mean_wd)   / mean_owen  * 100,
    pct_gain_vs_egidi = (mean_egidi - mean_wd)   / mean_egidi * 100,
    .groups = "drop"
  ) %>%
  mutate(
    pct_gain_vs_owen  = round(pct_gain_vs_owen,  1),
    pct_gain_vs_egidi = round(pct_gain_vs_egidi, 1)
  )

# take a look
print(df_pct_gain)
