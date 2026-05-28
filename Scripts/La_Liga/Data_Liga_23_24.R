#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(readr)
library(readxl)
library(lubridate) # For date handling

#   ____________________________________________________________________________
#   Data                                                                    ####
liga_20_21 <- read_csv("Data/Liga/liga_20_21.csv")
liga_21_22 <- read_csv("Data/Liga/liga_21_22.csv")
liga_22_23 <- read_csv("Data/Liga/liga_22_23.csv")
liga_23_24 <- read_csv("Data/Liga/liga_23_24.csv")

# Add season column to each
liga_20_21$season <- 2021
liga_21_22$season <- 2022
liga_22_23$season <- 2023
liga_23_24$season <- 2024

la_liga <- bind_rows(
  liga_20_21 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_21_22 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_22_23 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG),
  liga_23_24 %>% select(season, Date, HomeTeam, AwayTeam, FTHG, FTAG)
)


la_liga <- la_liga %>%
  mutate(Date = dmy(Date))

#   ____________________________________________________________________________
#   Compute periods                                                         ####


# Define key dates for each season
season_boundaries <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "16/10/2020",       "04/01/2021",        "05/01/2021",      "01/02/2021",
  2022,    "31/08/2021",       "20/12/2021",        "30/12/2021",      "31/01/2022",
  2023,    "01/09/2022",       "14/11/2022",        "25/12/2022",      "31/01/2023", # World Cup season
  2024,    "01/09/2023",       "11/01/2024",        "01/01/2024",      "01/02/2024"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

# Process la liga data with periods
la_liga <- la_liga %>%
  left_join(season_boundaries, by = "season") %>%
  group_by(season) %>%
  arrange(season, Date) %>%
  mutate(
    # Define season periods based on dates and season
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
    # Create global periods across all seasons
    periods = case_when(
      season == 2021 ~ season_period,
      season == 2022 ~ season_period + 2,
      season == 2023 ~ season_period + 4,
      season == 2024 ~ season_period + 6,
      TRUE ~ NA_real_
    )
  )


la_liga <- la_liga %>% select(periods, HomeTeam, AwayTeam, FTHG, FTAG)
colnames(la_liga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
