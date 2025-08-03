#   ____________________________________________________________________________
#   Libraries                                                               ####

# List of required packages
packages <- c("readr", "dplyr", "tidyr")

# Install packages if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}


# Load packages
invisible(lapply(packages, library, character.only = TRUE))

#   ____________________________________________________________________________
#   Data                                                                    ####

# Load World Cup data
ec_data <- read.csv2(file = "Data/results_euro.csv", sep = ",")

# Filter and select relevant data for training
ec_data_train <- ec_data %>%
  select(date, home_team, away_team, home_score, away_score, tournament) %>%
  filter(as.Date(date) > "2020-06-01" & as.Date(date) < "2024-06-12")

# Load FIFA ranking data
fifa_ranking <- read.csv2(file = "Data/ranking_fifa_april_2024.csv", sep = ",", header = TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points)) / (10 ^ 3)
fifa_ranking_clean <- fifa_ranking %>%
  filter(rank_date == "2024-04-04") %>%
  select(country_full, total_points)

# Update column names
colnames(fifa_ranking_clean) <- c("team_name", "ranking")

fifa_ranking_clean <- fifa_ranking_clean %>%
  mutate(team_name = ifelse(team_name == "Czechia", "Czech Republic", team_name))

#   ____________________________________________________________________________
#   Splitting the Dataset into 4 Periods                                    ####

# Redefining times from 1 to 4 based on the number of days in each period
ec_data_train <- ec_data_train %>%
  mutate(date = as.Date(date))

# Calculate total days span and divide into 4 periods
start_date <- min(ec_data_train$date)
end_date <- max(ec_data_train$date)
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))
days_per_period <- total_days / 4

ec_data_train <- ec_data_train %>%
  mutate(
    period = case_when(
      date >= start_date & date < (start_date + days_per_period) ~ 1,
      date >= (start_date + days_per_period) & date < (start_date + 2 * days_per_period) ~ 2,
      date >= (start_date + 2 * days_per_period) & date < (start_date + 3 * days_per_period) ~ 3,
      date >= (start_date + 3 * days_per_period) ~ 4
    )
  )

# Arrange the data by date
ec_data_train <- arrange(ec_data_train, date)

#   ____________________________________________________________________________
#   Training Set Preparation                                                ####

# Remove rows with missing scores
ec_data_train <- ec_data_train %>%
  filter(!is.na(home_score) & !is.na(away_score))

# Define teams to be removed based on FIFA ranking or low participation
low_teams <- c(
  "Haiti", "Sint Maarten", "CuraÃ§ao", "Grenada", "Cuba", "Turks and Caicos Islands",
  "Jersey", "Hitra", "Isle of Man", "Yorkshire", "Panjab", "Somaliland", "Kernow",
  "Barawa", "Chagos Islands", "Cascadia", "Parishes of Jersey", "Alderney",
  "Yoruba Nation", "Matabeleland", "Biafra", "Mapuche", "Maule Sur", "Aymara",
  "Saint Helena", "Shetland", "Ynys MÃ´n", "Orkney", "Guernsey", "Western Isles",
  "Timor-Leste", "Mayotte"
)

# Remove teams not present in FIFA ranking or in the low teams list
other_discarded_teams <- ec_data_train %>%
  filter(!(home_team %in% fifa_ranking_clean$team_name)) %>%
  pull(home_team)

to_remove <- c(low_teams, other_discarded_teams)
ec_data_train <- ec_data_train %>%
  filter(!(home_team %in% to_remove), !(away_team %in% to_remove))


#   ____________________________________________________________________________
#   Define Match Outcome                                                    ####

# Create a new column for match outcome
ec_data_train <- ec_data_train %>%
  mutate(
    match_outcome = case_when(
      home_score > away_score ~ 1,   # Home win
      home_score < away_score ~ 3,   # Away win
      TRUE ~ 2                       # Draw
    )
  )

#   ____________________________________________________________________________
#   Final Training Set                                                      ####

# Arrange the data by date
ec_data_train <- arrange(ec_data_train, date)

# Save the final training dataset
save(ec_data_train, file = "RData/StartingData_EC.RData")
