#   ____________________________________________________________________________
#   Libraries                                                               ####
library(ggplot2)
library(bayesplot)
library(dplyr)
library(footBayes)
library(readr)
library(lubridate)



#   ____________________________________________________________________________
#   Bookmaker definitions                                                   ####

bookmakers <- list(
  "1XBet"        = c("1XBH", "1XBD", "1XBA"),
  "Bet365"       = c("B365H", "B365D", "B365A"),
  "Betfair"      = c("BFH", "BFD", "BFA"),
  "Betfred"      = c("BFDH", "BFDD", "BFDA"),
  "BetMGM"       = c("BMGMH", "BMGMD", "BMGMA"),
  "Betvictor"    = c("BVH", "BVD", "BVA"),
  "BlueSquare"   = c("BSH", "BSD", "BSA"),
  "Coral"        = c("CLH", "CLD", "CLA"),
  "Gamebookers"  = c("GBH", "GBD", "GBA"),
  "Ladbrokes"    = c("LBH", "LBD", "LBA"),
  "Pinnacle"     = c("PSH", "PSD", "PSA"),
  "SportingOdds" = c("SOH", "SOD", "SOA"),
  "Sportingbet"  = c("SBH", "SBD", "SBA"),
  "StanJames"    = c("SJH", "SJD", "SJA"),
  "Stanleybet"   = c("SYH", "SYD", "SYA"),
  "VCBet"        = c("VCH", "VCD", "VCA")
)

odds_to_probs <- function(data, cols) {
  if (!all(cols %in% names(data))) {
    return(NULL)
  }
  odds <- data[, cols, drop = FALSE]
  if (all(is.na(odds))) {
    return(NULL)
  }
  implied <- 1 / odds
  probs <- as.matrix(implied / rowSums(implied))
  colnames(probs) <- c("Home", "Draw", "Away")
  return(probs)
}

#   ____________________________________________________________________________
#   Half season 2023/2024                                                   ####

##  ............................................................................
##  Bundesliga                                                              ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/Bundesliga/Data_Bundesliga_23_24.R")
bundesliga_23_24_full <- read_csv("Data/Bundesliga/bundesliga_23_24.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

bundesliga_23_24_test <- bundesliga_23_24_full %>% tail(163)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(bundesliga_23_24_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(bundesliga_23_24_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(bundesliga)
min <- max - 162
bundesliga_test <- bundesliga[min:max, ]

comparison_bundes_wt_bookmakers_23_24 <- compare_foot(
  source = bookmaker_probs,
  test_data = bundesliga_test
)

comparison_bundes_wt_bookmakers_23_24

save(comparison_bundes_wt_bookmakers_23_24, file = "RData/Bundesliga/comparison_bundes_wt_bookmakers_23_24.RData")


##  ............................................................................
##  Premier League                                                          ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####

source("Scripts/Premier_League/Data_Premier_23_24.R")
premier_23_24_full <- read_csv("Data/Premier_League/premier_23_24.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

premier_23_24_test <- premier_23_24_full %>% tail(184)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(premier_23_24_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(premier_23_24_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(premier)
min <- max - 183
premier_test <- premier[min:max, ]

comparison_premier_wt_bookmakers_23_24 <- compare_foot(
  source = bookmaker_probs,
  test_data = premier_test
)

comparison_premier_wt_bookmakers_23_24

save(comparison_premier_wt_bookmakers_23_24, file = "RData/Premier_League/comparison_premier_wt_bookmakers_23_24.RData")

##  ............................................................................
##  La Liga                                                                 ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####

source("Scripts/La_Liga/Data_Liga_23_24.R")
liga_23_24_full <- read_csv("Data/Liga/liga_23_24.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

liga_23_24_test <- liga_23_24_full %>% tail(190)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(liga_23_24_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(liga_23_24_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(la_liga)
min <- max - 189
liga_test <- la_liga[min:max, ]

comparison_liga_wt_bookmakers_23_24 <- compare_foot(
  source = bookmaker_probs,
  test_data = liga_test
)

comparison_liga_wt_bookmakers_23_24


save(comparison_liga_wt_bookmakers_23_24, file = "RData/La_Liga/comparison_liga_wt_bookmakers_23_24.RData")


#   ____________________________________________________________________________
#   Half season 2024/2025                                                   ####

##  ............................................................................
##  Bundesliga                                                              ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/Bundesliga/Data_Bundesliga.R")
bundesliga_24_25_full <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

bundesliga_24_25_test <- bundesliga_24_25_full %>% tail(171)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(bundesliga_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(bundesliga_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(bundesliga)
min <- max - 170
bundesliga_test <- bundesliga[min:max, ]

comparison_bundes_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = bundesliga_test
)

comparison_bundes_wt_bookmakers

save(comparison_bundes_wt_bookmakers, file = "RData/Bundesliga/comparison_bundes_wt_bookmakers.RData")


##  ............................................................................
##  Premier League                                                          ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/Premier_League/Data_Premier.R")
premier_24_25_full <- read_csv("Data/Premier_League/premier_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

premier_24_25_test <- premier_24_25_full %>% tail(192)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(premier_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(premier_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(premier)
min <- max - 191
premier_test <- premier[min:max, ]

comparison_premier_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = premier_test
)

comparison_premier_wt_bookmakers

save(comparison_premier_wt_bookmakers, file = "RData/Premier_League/comparison_premier_wt_bookmakers.RData")

##  ............................................................................
##  La Liga                                                                 ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/La_Liga/Data_Liga.R")
liga_24_25_full <- read_csv("Data/Liga/liga_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

liga_24_25_test <- liga_24_25_full %>% tail(199)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(liga_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(liga_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(la_liga)
min <- max - 198
liga_test <- la_liga[min:max, ]

comparison_liga_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = liga_test
)

comparison_liga_wt_bookmakers


save(comparison_liga_wt_bookmakers, file = "RData/La_Liga/comparison_liga_wt_bookmakers.RData")



#   ____________________________________________________________________________
#   Last matchday 2024/2025                                                 ####

##  ............................................................................
##  Bundesliga                                                              ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/Bundesliga/Data_Bundesliga.R")
bundesliga_24_25_full <- read_csv("Data/Bundesliga/bundesliga_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

bundesliga_24_25_test <- bundesliga_24_25_full %>% tail(9)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(bundesliga_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(bundesliga_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(bundesliga)
min <- max - 8
bundesliga_test <- bundesliga[min:max, ]

comparison_bundes_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = bundesliga_test
)

comparison_bundes_wt_bookmakers

save(comparison_bundes_wt_bookmakers, file = "RData/Bundesliga/comparison_bundes_wt_last_round_bookmakers.RData")


##  ............................................................................
##  Premier League                                                          ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/Premier_League/Data_Premier.R")
premier_24_25_full <- read_csv("Data/Premier_League/premier_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

premier_24_25_test <- premier_24_25_full %>% tail(10)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(premier_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(premier_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(premier)
min <- max - 9
premier_test <- premier[min:max, ]

comparison_premier_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = premier_test
)

comparison_premier_wt_bookmakers

save(comparison_premier_wt_bookmakers, file = "RData/Premier_League/comparison_premier_wt_last_round_bookmakers.RData")

##  ............................................................................
##  La Liga                                                                 ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Data                                                                    ####
source("Scripts/La_Liga/Data_Liga.R")
liga_24_25_full <- read_csv("Data/Liga/liga_24_25.csv")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Extract bookmaker probabilities                                         ####

liga_24_25_test <- liga_24_25_full %>% tail(10)

bookmaker_probs <- list()
for (bk_name in names(bookmakers)) {
  probs <- odds_to_probs(liga_24_25_test, bookmakers[[bk_name]])
  if (!is.null(probs)) {
    complete_rows <- complete.cases(probs)
    if (sum(!complete_rows) > 0) {
      message(sprintf(
        "%s: %d/%d rows with missing odds",
        bk_name, sum(!complete_rows), nrow(probs)
      ))
    }
    bookmaker_probs[[bk_name]] <- probs
  }
}

# Alternative Pinnacle columns
if (!"Pinnacle" %in% names(bookmaker_probs)) {
  probs <- odds_to_probs(liga_24_25_test, c("PH", "PD", "PA"))
  if (!is.null(probs)) bookmaker_probs[["Pinnacle"]] <- probs
}

# Market average
if (length(bookmaker_probs) > 1) {
  all_probs_array <- array(
    unlist(bookmaker_probs),
    dim = c(nrow(bookmaker_probs[[1]]), 3, length(bookmaker_probs))
  )
  market_avg <- apply(all_probs_array, c(1, 2), mean, na.rm = TRUE)
  market_avg <- market_avg / rowSums(market_avg)
  colnames(market_avg) <- c("Home", "Draw", "Away")
  bookmaker_probs[["MarketAvg"]] <- market_avg
}

cat("Available bookmakers:", paste(names(bookmaker_probs), collapse = ", "), "\n")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Comparison                                                              ####

max <- nrow(la_liga)
min <- max - 9
liga_test <- la_liga[min:max, ]

comparison_liga_wt_bookmakers <- compare_foot(
  source = bookmaker_probs,
  test_data = liga_test
)

comparison_liga_wt_bookmakers


save(comparison_liga_wt_bookmakers, file = "RData/La_Liga/comparison_liga_wt_last_round_bookmakers.RData")
