#' =============================================================================
#' Score-Level Evaluation Metrics for Goal-Based Football Models
#' =============================================================================
#' 
#' This script provides functions to evaluate goal-based models on their ability
#' to predict actual scores, not just match outcomes (1X2).
#' 
#' Addresses reviewer comment: "Since we have goal based models, I would expect 
#' to see a discussion (and some results) about how good we can predict the 
#' scores themselves."
#' =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

#' -----------------------------------------------------------------------------
#' METRIC 1: Mean Absolute Error (MAE) and RMSE for Goals
#' -----------------------------------------------------------------------------
#' Simple and interpretable: how far off are predicted goals on average?

compute_goal_errors <- function(fit, test_data) {
  # Extract posterior predictive samples
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])  # [S, N_prev, 2]
  
  S <- dim(y_prev)[1]  # number of posterior samples
  N <- dim(y_prev)[2]  # number of predicted matches
  
  # Posterior mean predictions
  pred_home <- colMeans(y_prev[, , 1])
  pred_away <- colMeans(y_prev[, , 2])
  
  # Actual goals
  actual_home <- test_data$home_goals
  actual_away <- test_data$away_goals
  
  # MAE
  mae_home <- mean(abs(pred_home - actual_home))
  mae_away <- mean(abs(pred_away - actual_away))
  mae_total <- mean(abs((pred_home + pred_away) - (actual_home + actual_away)))
  mae_diff <- mean(abs((pred_home - pred_away) - (actual_home - actual_away)))
  
  # RMSE
  rmse_home <- sqrt(mean((pred_home - actual_home)^2))
  rmse_away <- sqrt(mean((pred_away - actual_away)^2))
  rmse_total <- sqrt(mean(((pred_home + pred_away) - (actual_home + actual_away))^2))
  rmse_diff <- sqrt(mean(((pred_home - pred_away) - (actual_home - actual_away))^2))
  
  list(
    MAE = c(home = mae_home, away = mae_away, total = mae_total, diff = mae_diff),
    RMSE = c(home = rmse_home, away = rmse_away, total = rmse_total, diff = rmse_diff),
    predictions = data.frame(
      match = 1:N,
      pred_home = pred_home,
      pred_away = pred_away,
      actual_home = actual_home,
      actual_away = actual_away
    )
  )
}


#' -----------------------------------------------------------------------------
#' METRIC 2: Log-Score for Exact Scorelines
#' -----------------------------------------------------------------------------
#' Measures how well the model assigns probability to the observed scoreline.
#' This is the key metric that distinguishes goal-based from outcome-based models.

compute_score_logscore <- function(fit, test_data, max_goals = 10) {
  # Extract posterior predictive samples
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])  # [S, N_prev, 2]
  
  S <- dim(y_prev)[1]
  N <- dim(y_prev)[2]
  
  log_scores <- numeric(N)
  scoreline_probs <- vector("list", N)
  
  for (n in 1:N) {
    home_samples <- y_prev[, n, 1]
    away_samples <- y_prev[, n, 2]
    
    actual_home <- test_data$home_goals[n]
    actual_away <- test_data$away_goals[n]
    
    # Compute probability of exact scoreline from posterior samples
    prob_exact <- mean(home_samples == actual_home & away_samples == actual_away)
    
    # Avoid log(0)
    prob_exact <- max(prob_exact, 1e-10)
    log_scores[n] <- log(prob_exact)
    
    # Also store full scoreline probability matrix for this match
    score_tab <- table(
      factor(home_samples, levels = 0:max_goals),
      factor(away_samples, levels = 0:max_goals)
    ) / S
    scoreline_probs[[n]] <- score_tab
  }
  
  list(
    log_score = mean(log_scores),  # Average log-score (higher is better)
    log_scores_per_match = log_scores,
    scoreline_probs = scoreline_probs
  )
}


#' -----------------------------------------------------------------------------
#' METRIC 3: Ranked Probability Score for Scores (Score-RPS)
#' -----------------------------------------------------------------------------
#' Extends RPS to the full goal distribution, not just outcomes.
#' Penalizes predictions that are "far" from the observed goals.

compute_score_rps <- function(fit, test_data, max_goals = 10) {
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])
  
  S <- dim(y_prev)[1]
  N <- dim(y_prev)[2]
  
  rps_home <- numeric(N)
  rps_away <- numeric(N)
  
  for (n in 1:N) {
    home_samples <- y_prev[, n, 1]
    away_samples <- y_prev[, n, 2]
    
    actual_home <- test_data$home_goals[n]
    actual_away <- test_data$away_goals[n]
    
    # Compute CDF for home goals
    pred_cdf_home <- sapply(0:max_goals, function(g) mean(home_samples <= g))
    actual_cdf_home <- as.numeric(0:max_goals >= actual_home)
    rps_home[n] <- sum((pred_cdf_home - actual_cdf_home)^2)
    
    # Compute CDF for away goals
    pred_cdf_away <- sapply(0:max_goals, function(g) mean(away_samples <= g))
    actual_cdf_away <- as.numeric(0:max_goals >= actual_away)
    rps_away[n] <- sum((pred_cdf_away - actual_cdf_away)^2)
  }
  
  list(
    score_rps_home = mean(rps_home),
    score_rps_away = mean(rps_away),
    score_rps_total = mean(rps_home + rps_away),
    rps_per_match = data.frame(match = 1:N, rps_home = rps_home, rps_away = rps_away)
  )
}


#' -----------------------------------------------------------------------------
#' METRIC 4: Calibration Analysis
#' -----------------------------------------------------------------------------
#' Check if predicted goal probabilities are well-calibrated.
#' Group predictions by predicted probability and compare to observed frequency.

compute_goal_calibration <- function(fit, test_data, max_goals = 8) {
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])
  
  S <- dim(y_prev)[1]
  N <- dim(y_prev)[2]
  
  # Collect all (predicted_prob, actual_outcome) pairs for each goal count
  calibration_data <- list()
  
  for (g in 0:max_goals) {
    pred_probs_home <- sapply(1:N, function(n) mean(y_prev[, n, 1] == g))
    pred_probs_away <- sapply(1:N, function(n) mean(y_prev[, n, 2] == g))
    
    actual_home <- as.numeric(test_data$home_goals == g)
    actual_away <- as.numeric(test_data$away_goals == g)
    
    calibration_data[[paste0("home_", g)]] <- data.frame(
      goals = g, type = "home",
      pred_prob = pred_probs_home, 
      actual = actual_home
    )
    calibration_data[[paste0("away_", g)]] <- data.frame(
      goals = g, type = "away",
      pred_prob = pred_probs_away, 
      actual = actual_away
    )
  }
  
  cal_df <- bind_rows(calibration_data)
  
  # Bin by predicted probability and compute observed frequency
  cal_df$prob_bin <- cut(cal_df$pred_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  cal_summary <- cal_df %>%
    group_by(prob_bin) %>%
    summarise(
      mean_pred = mean(pred_prob),
      mean_actual = mean(actual),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(prob_bin))
  
  list(
    calibration_data = cal_df,
    calibration_summary = cal_summary
  )
}


#' -----------------------------------------------------------------------------
#' WRAPPER: Compute All Score-Level Metrics
#' -----------------------------------------------------------------------------

compare_foot_scores <- function(source, test_data, max_goals = 10) {
  
  results <- list()
  
  for (item_name in names(source)) {
    item <- source[[item_name]]
    
    if (!inherits(item, "stanFoot")) {
      warning(sprintf("Skipping '%s': not a stanFoot object", item_name))
      next
    }
    
    cat(sprintf("Computing score-level metrics for: %s\n", item_name))
    
    # Goal errors (MAE/RMSE)
    errors <- compute_goal_errors(item, test_data)
    
    # Log-score for exact scorelines
    logscore <- compute_score_logscore(item, test_data, max_goals)
    
    # Score-level RPS
    score_rps <- compute_score_rps(item, test_data, max_goals)
    
    results[[item_name]] <- list(
      MAE_home = errors$MAE["home"],
      MAE_away = errors$MAE["away"],
      MAE_total = errors$MAE["total"],
      MAE_diff = errors$MAE["diff"],
      RMSE_home = errors$RMSE["home"],
      RMSE_away = errors$RMSE["away"],
      RMSE_total = errors$RMSE["total"],
      RMSE_diff = errors$RMSE["diff"],
      log_score = logscore$log_score,
      score_RPS_home = score_rps$score_rps_home,
      score_RPS_away = score_rps$score_rps_away,
      score_RPS_total = score_rps$score_rps_total
    )
  }
  
  # Convert to data frame
  metrics_df <- do.call(rbind, lapply(names(results), function(nm) {
    data.frame(Model = nm, as.data.frame(results[[nm]]))
  }))
  rownames(metrics_df) <- NULL
  
  class(metrics_df) <- c("compareFootScores", "data.frame")
  return(metrics_df)
}


#' -----------------------------------------------------------------------------
#' VISUALIZATION: Scoreline Probability Heatmap
#' -----------------------------------------------------------------------------

plot_scoreline_heatmap <- function(fit, test_data, match_idx = 1, max_goals = 6) {
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])
  
  S <- dim(y_prev)[1]
  
  home_samples <- y_prev[, match_idx, 1]
  away_samples <- y_prev[, match_idx, 2]
  
  # Compute scoreline probabilities
  score_probs <- expand.grid(home = 0:max_goals, away = 0:max_goals)
  score_probs$prob <- sapply(1:nrow(score_probs), function(i) {
    mean(home_samples == score_probs$home[i] & away_samples == score_probs$away[i])
  })
  
  actual_home <- test_data$home_goals[match_idx]
  actual_away <- test_data$away_goals[match_idx]
  
  ggplot(score_probs, aes(x = factor(away), y = factor(home), fill = prob)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", prob)), size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue", name = "Probability") +
    geom_point(data = data.frame(away = factor(actual_away), home = factor(actual_home)),
               aes(x = away, y = home), color = "red", size = 4, shape = 4, stroke = 2,
               inherit.aes = FALSE) +
    labs(
      title = sprintf("Match %d: %s vs %s", match_idx,
                      test_data$home_team[match_idx], 
                      test_data$away_team[match_idx]),
      subtitle = sprintf("Actual: %d-%d (marked with X)", actual_home, actual_away),
      x = "Away Goals",
      y = "Home Goals"
    ) +
    theme_minimal() +
    theme(aspect.ratio = 1)
}


#' -----------------------------------------------------------------------------
#' VISUALIZATION: Goal Distribution Comparison
#' -----------------------------------------------------------------------------

plot_goal_distribution <- function(fit, test_data, type = "home", max_goals = 8) {
  draws <- fit$fit$draws()
  draws <- posterior::as_draws_rvars(draws)
  y_prev <- posterior::draws_of(draws[["y_prev"]])
  
  col_idx <- if (type == "home") 1 else 2
  actual_goals <- if (type == "home") test_data$home_goals else test_data$away_goals
  
  # Aggregate predicted distribution across all matches
  all_samples <- as.vector(y_prev[, , col_idx])
  pred_dist <- table(factor(all_samples, levels = 0:max_goals)) / length(all_samples)
  
  # Observed distribution
  obs_dist <- table(factor(actual_goals, levels = 0:max_goals)) / length(actual_goals)
  
  plot_df <- data.frame(
    goals = 0:max_goals,
    Predicted = as.numeric(pred_dist),
    Observed = as.numeric(obs_dist)
  ) %>%
    pivot_longer(cols = c(Predicted, Observed), names_to = "Type", values_to = "Proportion")
  
  ggplot(plot_df, aes(x = factor(goals), y = Proportion, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = sprintf("Goal Distribution: %s Goals", tools::toTitleCase(type)),
      x = "Goals", y = "Proportion"
    ) +
    scale_fill_manual(values = c("Predicted" = "steelblue", "Observed" = "coral")) +
    theme_minimal()
}


#' =============================================================================
#' EXAMPLE USAGE
#' =============================================================================
#' 
#' # After fitting models as in your compare_foot.R:
#' 
#' # 1. Compute score-level metrics
#' score_metrics <- compare_foot_scores(
#'   source = list(
#'     double_pois_comm = double_pois_comm,
#'     double_pois_owen = double_pois_owen,
#'     biv_pois_comm = biv_pois_comm
#'   ),
#'   test_data = la_liga_test
#' )
#' print(score_metrics)
#' 
#' # 2. Visualize scoreline predictions for a specific match
#' plot_scoreline_heatmap(double_pois_comm, la_liga_test, match_idx = 5)
#' 
#' # 3. Compare goal distributions
#' plot_goal_distribution(double_pois_comm, la_liga_test, type = "home")
#' 
#' =============================================================================


#' =============================================================================
#' TABLE FORMATTING FOR PAPER
#' =============================================================================

format_score_metrics_table <- function(score_metrics, digits = 3) {
  # Select key columns and format
  key_cols <- c("Model", "MAE_home", "MAE_away", "RMSE_home", "RMSE_away", 
                "log_score", "score_RPS_total")
  
  out <- score_metrics[, key_cols]
  
  # Round numeric columns
  num_cols <- sapply(out, is.numeric)
  out[num_cols] <- lapply(out[num_cols], round, digits = digits)
  
  # Rename for paper
  names(out) <- c("Model", "MAE (H)", "MAE (A)", "RMSE (H)", "RMSE (A)", 
                  "Log-Score", "Score-RPS")
  
  return(out)
}