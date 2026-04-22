# ============================================
# 05 — SIGNIFICANCE TESTS
# ============================================

# Load dependencies
load("dissertation_models.RData")
load("dissertation_results.RData")
load("dissertation_returns.RData")
library(xts)
library(zoo)
# Function to calculate t-statistic for a CAR
calc_t_stat <- function(merged, event_date, model, 
                        est_start = -120, est_end = -20,
                        evt_start = -1, evt_end = 1) {
  
  all_dates <- index(merged)
  event_pos <- which(all_dates >= event_date)[1]
  
  # Extract estimation window
  est_data <- merged[(event_pos + est_start):(event_pos + est_end), ]
  
  # Get residuals from market model over estimation window
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  expected_est <- alpha + beta * est_data[, 2]
  residuals_est <- est_data[, 1] - expected_est
  
  # Standard deviation of residuals
  sigma <- sd(residuals_est)
  
  # Number of days in event window
  n_days <- evt_end - evt_start + 1
  
  # Standard error of CAR
  se_CAR <- sigma * sqrt(n_days)
  
  return(se_CAR)
}

exists("calc_t_stat")

# Calculate t-statistics for all firms and events
significance_results <- list()

for (event_name in names(event_dates)) {
  event_date <- event_dates[event_name]
  significance_results[[event_name]] <- list()
  
  for (firm_name in names(merged_data)) {
    
    # Get CAR
    CAR <- CAR_results[[event_name]][[firm_name]]
    
    # Get standard error
    se <- calc_t_stat(
      merged     = merged_data[[firm_name]],
      event_date = event_date,
      model      = market_models[[event_name]][[firm_name]],
      est_start  = -120,
      est_end    = -20,
      evt_start  = -1,
      evt_end    = 1
    )
    
    # Calculate t-statistic
    t_stat <- CAR / se
    
    # Calculate p-value (two-tailed)
    p_value <- 2 * (1 - pt(abs(t_stat), df = 98))
    
    # Significance stars
    stars <- ifelse(p_value < 0.01, "***",
                    ifelse(p_value < 0.05, "**",
                           ifelse(p_value < 0.10, "*", "")))
    
    significance_results[[event_name]][[firm_name]] <- list(
      CAR     = CAR,
      se      = se,
      t_stat  = t_stat,
      p_value = p_value,
      stars   = stars
    )
  }
}

# Print final results table
cat("\nEVENT STUDY RESULTS — CARs WITH SIGNIFICANCE\n")
cat("Estimation window: (-120, -20) | Event window: (-1, +1)\n")
cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
            "Event", "AZN", "SAN", "BMY", "LLY", "DS"))
cat(rep("-", 80), "\n")

for (event_name in names(significance_results)) {
  # CARs row
  cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
              event_name,
              paste0(round(significance_results[[event_name]]$AZN$CAR * 100, 2), "%",
                     significance_results[[event_name]]$AZN$stars),
              paste0(round(significance_results[[event_name]]$SAN$CAR * 100, 2), "%",
                     significance_results[[event_name]]$SAN$stars),
              paste0(round(significance_results[[event_name]]$BMY$CAR * 100, 2), "%",
                     significance_results[[event_name]]$BMY$stars),
              paste0(round(significance_results[[event_name]]$LLY$CAR * 100, 2), "%",
                     significance_results[[event_name]]$LLY$stars),
              paste0(round(significance_results[[event_name]]$DS$CAR * 100, 2), "%",
                     significance_results[[event_name]]$DS$stars)))
  
  # T-statistics row
  cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
              "",
              paste0("(", round(significance_results[[event_name]]$AZN$t_stat, 3), ")"),
              paste0("(", round(significance_results[[event_name]]$SAN$t_stat, 3), ")"),
              paste0("(", round(significance_results[[event_name]]$BMY$t_stat, 3), ")"),
              paste0("(", round(significance_results[[event_name]]$LLY$t_stat, 3), ")"),
              paste0("(", round(significance_results[[event_name]]$DS$t_stat, 3), ")")))
  cat("\n")
}

cat("Note: t-statistics in parentheses\n")
cat("*** p<0.01  ** p<0.05  * p<0.10\n")

# Save significance results
save(significance_results, 
     file = "dissertation_significance.RData")