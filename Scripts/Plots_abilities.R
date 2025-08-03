#   ____________________________________________________________________________
#   Libraries                                                               ####

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(forcats)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("~/Desktop/Work/Projects/HBGM/RData/La_Liga/Abilities/df_abilities_liga_half_10.RData")
load("~/Desktop/Work/Projects/HBGM/RData/Bundesliga/Abilities/df_abilities_bundesliga_half_10.RData")
load("~/Desktop/Work/Projects/HBGM/RData/Premier_League/Abilities/df_abilities_premier_half_10.RData")

bundesliga <- df_abilities_bundesliga_half_10
liga <- df_abilities_liga_half_10
premier <- df_abilities_premier_half_10


combined <- bind_rows(bundesliga, liga, premier) %>%
  mutate(
    model_type = case_when(
      str_detect(variant, "Comm") ~ "Weighted Dynamic",
      str_detect(variant, "Egidi") ~ "Egidi et al. (2018)",
      str_detect(variant, "Owen") ~ "Owen (2011)",
      TRUE ~ "Dynamic"
    )
  ) %>%
  # re-level model_type so facets come out in the order you want:
  mutate(
    model_type = factor(
      model_type,
      levels = c("Weighted Dynamic", "Owen (2011)", "Egidi et al. (2018)", "Dynamic")
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
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Period",
    y = "Ability values"
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
  filename = "ability_plot_half_10.pdf", path = "Plots",
  plot = ability_plot,
  width = 17, height = 12, device = "pdf", dpi = 500, useDingbats = FALSE
)
