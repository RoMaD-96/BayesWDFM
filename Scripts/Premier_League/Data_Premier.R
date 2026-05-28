#   ____________________________________________________________________________
#   Libraries                                                               ####
library(dplyr)
library(readr)
library(readxl)
library(lubridate) # For date handling

#   ____________________________________________________________________________
#   Data                                                                    ####
premier_20_21 <- read_csv("Data/Premier_League/premier_20_21.csv")
premier_21_22 <- read_csv("Data/Premier_League/premier_21_22.csv")
premier_22_23 <- read_csv("Data/Premier_League/premier_22_23.csv")
premier_23_24 <- read_csv("Data/Premier_League/premier_23_24.csv")
premier_24_25 <- read_csv("Data/Premier_League/premier_24_25.csv")

# Add season column to each
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
)

premier <- premier %>%
  mutate(Date = dmy(Date))

#   ____________________________________________________________________________
#   Compute periods.                                                        ####

# Define key dates for each season
season_boundaries <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "16/10/2020",       "01/01/2021",        "02/01/2021",      "01/02/2021",
  2022,    "31/08/2021",       "31/12/2021",        "01/01/2022",      "31/01/2022",
  2023,    "01/09/2022",       "14/11/2022",        "25/12/2022",      "31/01/2023", # World Cup season
  2024,    "01/09/2023",       "31/12/2023",        "01/01/2024",      "01/02/2024",
  2025,    "30/08/2024",       "31/12/2024",        "01/01/2025",      "03/02/2025"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

# Process premier league data with periods
premier <- premier %>%
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
      season == 2025 ~ season_period + 8,
      TRUE ~ NA_real_
    )
  )


premier <- premier %>% select(periods, HomeTeam, AwayTeam, FTHG, FTAG)
colnames(premier) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
