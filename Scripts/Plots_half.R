#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

#   ____________________________________________________________________________
#   Computational Time Plot                                                 ####

load("RData/Bundesliga/time_results_bundes_half.RData")
load("RData/La_Liga/time_results_liga_half.RData")
load("RData/Premier_League/time_results_premier_half.RData")

df_bundes <- time_results_bundes_half %>% mutate(league = "Bundesliga")
df_liga <- time_results_liga_half %>% mutate(league = "La Liga")
df_epl <- time_results_premier_half %>% mutate(league = "EPL")

df_all <- bind_rows(df_bundes, df_liga, df_epl)

df_all <- df_all %>%
  mutate(
    model_type = case_when(
      str_detect(model, "_comm$") ~ "Weighted Dynamic",
      str_detect(model, "_owen$") ~ "Owen (2011)",
      TRUE ~ "Egidi et al. (2018)"
    ),
    # force the factor levels in the desired order
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)")
    ),
    goal_model = case_when(
      str_detect(model, "^biv_pois") ~ "Bivariate Poisson",
      str_detect(model, "^diag_biv_pois") ~ "Diag. Infl. Biv. Poisson",
      str_detect(model, "^double_pois") ~ "Double Poisson",
      str_detect(model, "^neg_bin") ~ "Negative Binomial",
      str_detect(model, "^skellam") ~ "Skellam",
      str_detect(model, "^zero_skellam") ~ "Zero-inflated Skellam",
      TRUE ~ NA_character_
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
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "grey95"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  labs(
    x     = NULL,
    y     = "Elapsed Time (seconds)"
  )


ggsave("comp_time_half.pdf",
  plot = comp_time_half, path = "Plots",
  width = 12, height = 10, device = "pdf", dpi = 500, useDingbats = FALSE
)



df_pct_gain <- df_all %>%
  group_by(league, goal_model) %>%
  summarise(
    mean_wd = mean(elapsed[model_type == "Weighted Dynamic"], na.rm = TRUE),
    mean_owen = mean(elapsed[model_type == "Owen (2011)"], na.rm = TRUE),
    mean_egidi = mean(elapsed[model_type == "Egidi et al. (2018)"], na.rm = TRUE),
    # % gain relative to each original method:
    pct_gain_vs_owen = (mean_owen - mean_wd) / mean_owen * 100,
    pct_gain_vs_egidi = (mean_egidi - mean_wd) / mean_egidi * 100,
    .groups = "drop"
  ) %>%
  mutate(
    pct_gain_vs_owen  = round(pct_gain_vs_owen, 1),
    pct_gain_vs_egidi = round(pct_gain_vs_egidi, 1)
  )

print(df_pct_gain)
