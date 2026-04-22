# ============================================
# 06 — ROBUSTNESS CHECKS
# Alternative event window: (-2, +2)
# ============================================

load("dissertation_models.RData")
load("dissertation_results.RData")
load("dissertation_returns.RData")
library(xts)
library(zoo)

# Reuse calc_abnormal_returns and calc_t_stat from script 05
# but with evt_start = -2, evt_end = 2

# Calculate abnormal returns with wider window
abnormal_returns_22 <- list()

for (event_name in names(event_dates)) {
  event_date <- event_dates[event_name]
  abnormal_returns_22[[event_name]] <- list()
  
  for (firm_name in names(merged_data)) {
    ar <- calc_abnormal_returns(
      merged     = merged_data[[firm_name]],
      event_date = event_date,
      model      = market_models[[event_name]][[firm_name]],
      evt_start  = -2,
      evt_end    = 2
    )
    abnormal_returns_22[[event_name]][[firm_name]] <- ar
  }
}

# Calculate CARs for (-2, +2)
CAR_results_22 <- list()

for (event_name in names(abnormal_returns_22)) {
  CAR_results_22[[event_name]] <- list()
  
  for (firm_name in names(abnormal_returns_22[[event_name]])) {
    ar <- abnormal_returns_22[[event_name]][[firm_name]]
    if (!is.null(ar)) {
      CAR_results_22[[event_name]][[firm_name]] <- sum(ar)
    } else {
      CAR_results_22[[event_name]][[firm_name]] <- NA
    }
  }
}

# Calculate significance for (-2, +2)
significance_results_22 <- list()

for (event_name in names(event_dates)) {
  event_date <- event_dates[event_name]
  significance_results_22[[event_name]] <- list()
  
  for (firm_name in names(merged_data)) {
    
    CAR <- CAR_results_22[[event_name]][[firm_name]]
    
    se <- calc_t_stat(
      merged     = merged_data[[firm_name]],
      event_date = event_date,
      model      = market_models[[event_name]][[firm_name]],
      est_start  = -120,
      est_end    = -20,
      evt_start  = -2,
      evt_end    = 2
    )
    
    t_stat  <- CAR / se
    p_value <- 2 * (1 - pt(abs(t_stat), df = 98))
    stars   <- ifelse(p_value < 0.01, "***",
                      ifelse(p_value < 0.05, "**",
                             ifelse(p_value < 0.10, "*", "")))
    
    significance_results_22[[event_name]][[firm_name]] <- list(
      CAR     = CAR,
      se      = se,
      t_stat  = t_stat,
      p_value = p_value,
      stars   = stars
    )
  }
}

# Print results table
cat("\nROBUSTNESS CHECK — EVENT WINDOW (-2, +2)\n")
cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
            "Event", "AZN", "SAN", "BMY", "LLY", "DS"))
cat(rep("-", 80), "\n")

for (event_name in names(significance_results_22)) {
  cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
              event_name,
              paste0(round(significance_results_22[[event_name]]$AZN$CAR * 100, 2), "%",
                     significance_results_22[[event_name]]$AZN$stars),
              paste0(round(significance_results_22[[event_name]]$SAN$CAR * 100, 2), "%",
                     significance_results_22[[event_name]]$SAN$stars),
              paste0(round(significance_results_22[[event_name]]$BMY$CAR * 100, 2), "%",
                     significance_results_22[[event_name]]$BMY$stars),
              paste0(round(significance_results_22[[event_name]]$LLY$CAR * 100, 2), "%",
                     significance_results_22[[event_name]]$LLY$stars),
              paste0(round(significance_results_22[[event_name]]$DS$CAR * 100, 2), "%",
                     significance_results_22[[event_name]]$DS$stars)))
  
  cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
              "",
              paste0("(", round(significance_results_22[[event_name]]$AZN$t_stat, 3), ")"),
              paste0("(", round(significance_results_22[[event_name]]$SAN$t_stat, 3), ")"),
              paste0("(", round(significance_results_22[[event_name]]$BMY$t_stat, 3), ")"),
              paste0("(", round(significance_results_22[[event_name]]$LLY$t_stat, 3), ")"),
              paste0("(", round(significance_results_22[[event_name]]$DS$t_stat, 3), ")")))
  cat("\n")
}

cat("Note: t-statistics in parentheses\n")
cat("*** p<0.01  ** p<0.05  * p<0.10\n")

save(abnormal_returns_22, CAR_results_22, significance_results_22,
     file = "dissertation_robustness.RData")