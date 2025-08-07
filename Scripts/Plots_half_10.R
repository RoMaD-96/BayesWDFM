#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

#   ____________________________________________________________________________
#   Load                                                                    ####

load("RData/La_Liga/comparison_liga_half_10.RData")
load("RData/Premier_League/comparison_premier_half_10.RData")
load("RData/Bundesliga/comparison_bundes_half_9.RData")
# Merging data


comparison_bundes_half_9$metrics$league <- "Bundesliga"
comparison_liga_half_10$metrics$league <- "La Liga"
comparison_premier_half_10$metrics$league <- "EPL"

bundesliga <- comparison_bundes_half_9$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )
liga <- comparison_liga_half_10$metrics %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )

premier <- comparison_premier_half_10$metrics %>%
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
      str_detect(Model, "_comm$") ~ "Weighted Dynamic",
      str_detect(Model, "_egidi$") ~ "Egidi et al. (2018)",
      str_detect(Model, "_owen$") ~ "Owen (2011)",
      TRUE ~ "Dynamic"
    ),
    # 2) strip off ANY of the three suffixes
    base = str_remove(Model, "_(comm|egidi|owen)$"),
    # 3) map to your human‐readable goal names
    goal_model = case_when(
      base == "biv_pois" ~ "Bivariate Poisson",
      base == "diag_biv_pois" ~ "Diagonal Bivariate Poisson",
      base == "double_pois" ~ "Double Poisson",
      base == "neg_bin" ~ "Negative Binomial",
      base == "skellam" ~ "Skellam",
      base == "zero_skellam" ~ "Zero-Inflated Skellam",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-base)
head(combined)

# Transform dataset in long

df_long_scoring_rules <- combined %>%
  pivot_longer(
    cols      = c(RPS, brier),
    names_to  = "measure",
    values_to = "score"
  ) %>%
  # reorder and relabel the measure factor
  mutate(
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)")
    ),
    measure = factor(
      measure,
      levels = c("RPS", "brier"),
      labels = c("RPS", "Brier Score")
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


df_long_mean_based <- combined %>%
  pivot_longer(
    cols      = c(ACP, pseudoR2),
    names_to  = "measure",
    values_to = "score"
  ) %>%
  # reorder and relabel the measure factor
  mutate(
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)")
    ),
    measure = factor(
      measure,
      levels = c("ACP", "pseudoR2"),
      labels = c("ACP", "Pseudo R^2")
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


df_all <- bind_rows(
  df_long_scoring_rules, # has RPS + Brier Score
  df_long_mean_based # has ACP + Pseudo R^2
)

my_cols <- c(
  "Weighted Dynamic"    = "#E69F00",
  "Owen (2011)"         = "#440154",
  "Egidi et al. (2018)" = "#29AF7F"
)
my_shapes <- c(
  "Weighted Dynamic"    = 16,
  "Owen (2011)"         = 18,
  "Egidi et al. (2018)" = 15
)

# Plot for Brier Score & ACP
plot_brier_acp <- df_all %>%
  filter(measure %in% c("Brier Score", "ACP")) %>%
  ggplot(aes(
    x     = goal_model,
    y     = score,
    group = model_type,
    color = model_type,
    shape = model_type
  )) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  facet_grid(measure ~ league, scales = "free_y") +
  scale_color_manual(values = my_cols) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Model",
    y = "Values"
  ) +
  theme_bw() +
  theme(
    legend.position  = "top",
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 14),
    strip.text       = element_text(size = 14),
    strip.background = element_rect(fill = "grey95"),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12)
  )

# Plot for RPS & Pseudo R^2
plot_rps_pseudoR2 <- df_all %>%
  filter(measure %in% c("RPS", "Pseudo R^2")) %>%
  ggplot(aes(
    x     = goal_model,
    y     = score,
    group = model_type,
    color = model_type,
    shape = model_type
  )) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  facet_grid(measure ~ league, scales = "free_y") +
  scale_color_manual(values = my_cols) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Model",
    y = "Values"
  ) +
  theme_bw() +
  theme(
    legend.position  = "top",
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 14),
    strip.text       = element_text(size = 14),
    strip.background = element_rect(fill = "grey95"),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12)
  )

print(plot_brier_acp)
print(plot_rps_pseudoR2)

ggsave("plot_brier_acp_half10.pdf",
  plot = plot_brier_acp, path = "Plots",
  width = 12, height = 8, device = "pdf", dpi = 500, useDingbats = FALSE
)
ggsave("plot_rps_pseudoR2_half10.pdf",
  plot = plot_rps_pseudoR2, path = "Plots",
  width = 12, height = 8, device = "pdf", dpi = 500, useDingbats = FALSE
)



#   ____________________________________________________________________________
#   Computational Time Plot                                                 ####

load("~/Desktop/Work/Projects/HBGM/RData/Bundesliga/time_results_bundes_half_9.RData")    
load("~/Desktop/Work/Projects/HBGM/RData/La_Liga/time_results_liga_half_10.RData") 
load("~/Desktop/Work/Projects/HBGM/RData/Premier_League/time_results_premier_half_10.RData")

df_bundes <- time_results_bundes_half_9    %>% mutate(league = "Bundesliga")
df_liga   <- time_results_liga_half_10     %>% mutate(league = "La Liga")
df_epl    <- time_results_premier_half_10  %>% mutate(league = "EPL")

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


comp_time_half_10 <- ggplot(df_all, aes(x = model_type, y = elapsed, fill = model_type)) +
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


ggsave("comp_time_half_10.pdf",
       plot = comp_time_half_10, path = "Plots",
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
