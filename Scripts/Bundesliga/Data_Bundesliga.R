#   ____________________________________________________________________________
#   Libraries                                                               ####
library(dplyr)
library(readr)
library(lubridate)

#   ____________________________________________________________________________
#   Data                                                                    ####
bundesliga_20_21 <- read_csv("Data/Bundesliga/bundesliga_20_21.csv")
bundesliga_21_22 <- read_csv("Data/Bundesliga/bundesliga_21_22.csv")
bundesliga_22_23 <- read_csv("Data/Bundesliga/bundesliga_22_23.csv")
bundesliga_23_24 <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")
bundesliga_24_25 <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")

# Add season column to each
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
)

# Parse dates
bundesliga <- bundesliga %>%
  mutate(Date = dmy(Date))

#   ____________________________________________________________________________
#   Define periods based on football calendar events                       ####

# Define key dates for each season
season_boundaries <- tibble::tribble(
  ~season, ~summer_window_end, ~winter_break_start, ~winter_break_end, ~winter_window_end,
  2021,    "05/10/2020",       "23/12/2020",        "01/01/2021",      "01/02/2021",
  2022,    "01/09/2021",       "23/12/2021",        "07/01/2022",      "31/01/2022",
  2023,    "01/09/2022",       "13/11/2022",        "20/01/2023",      "31/01/2023", # World Cup season
  2024,    "01/09/2023",       "22/12/2023",        "12/01/2024",      "01/02/2024",
  2025,    "30/08/2024",       "21/12/2024",        "10/01/2025",      "03/02/2025"
) %>%
  mutate(
    summer_window_end   = dmy(summer_window_end),
    winter_break_start  = dmy(winter_break_start),
    winter_break_end    = dmy(winter_break_end),
    winter_window_end   = dmy(winter_window_end)
  )

# Process bundesliga data with periods
bundesliga <- bundesliga %>%
  left_join(season_boundaries, by = "season") %>%
  group_by(season) %>%
  arrange(season, Date) %>%
  mutate(
    match_id = row_number(),
    matchweek = ceiling(match_id / 9),
    # Define season periods based on dates
    season_period = case_when(
      Date < winter_break_end ~ 1, # Everything before break ends
      Date >= winter_break_end ~ 2 # Everything after break ends
    )
  ) %>%
  ungroup() %>%
  arrange(season, Date) %>%
  mutate(
    periods = (season - min(season)) * 2 + season_period
  ) %>%
  select(periods, HomeTeam, AwayTeam, FTHG, FTAG)

colnames(bundesliga) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
