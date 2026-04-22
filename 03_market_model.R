# ============================================
# 03 — MARKET MODEL ESTIMATION
# ============================================

# Load dependencies
load("dissertation_returns.RData")
library(xts)
library(zoo)

# Primary event dates
event_dates <- as.Date(c(
  "2009-05-11",  # E1 - AZ PLATO press release
  "2009-07-10",  # E2 - Prasugrel FDA approval
  "2009-09-10",  # E3 - PLATO NEJM publication
  "2010-07-28",  # E4 - FDA Advisory Committee 7-1 vote
  "2010-12-03",  # E5 - EMA marketing authorisation
  "2010-12-15",  # E6 - FDA Complete Response Letter
  "2011-07-20"   # E7 - FDA final approval Brilinta
))

names(event_dates) <- c("E1_PLATO_PR", "E2_Prasugrel_FDA", 
                        "E3_PLATO_NEJM", "E4_FDA_AdCom",
                        "E5_EMA_Auth", "E6_FDA_CRL", 
                        "E7_FDA_Approval")

print(event_dates)

# Pair each firm with its domestic index
firm_index_pairs <- list(
  AZN = list(firm = ret_AZN, index = ret_FTSE),
  SAN = list(firm = ret_SAN, index = ret_CAC40),
  BMY = list(firm = ret_BMY, index = ret_SP500),
  LLY = list(firm = ret_LLY, index = ret_SP500),
  DS  = list(firm = ret_DS,  index = ret_NK225)
)

# Function to merge firm and index on common trading days
merge_firm_index <- function(firm_ret, index_ret) {
  merged <- merge(firm_ret, index_ret, join = "inner")
  merged <- na.omit(merged)
  return(merged)
}

# Apply to all firms
merged_data <- lapply(firm_index_pairs, function(x) {
  merge_firm_index(x$firm, x$index)
})

# Check dimensions
lapply(merged_data, nrow)

# Function to estimate market model for a given firm-index pair and event date
estimate_market_model <- function(merged, event_date, est_start = -120, est_end = -20) {
  
  # Get all trading dates for this firm
  all_dates <- index(merged)
  
  # Find position of event date or nearest trading day after
  event_pos <- which(all_dates >= event_date)[1]
  
  # Check event date exists in data
  if (is.na(event_pos)) {
    message("Event date out of range")
    return(NULL)
  }
  
  # Define estimation window positions
  est_start_pos <- event_pos + est_start
  est_end_pos   <- event_pos + est_end
  
  # Check estimation window is within data range
  if (est_start_pos < 1) {
    message("Estimation window out of range")
    return(NULL)
  }
  
  # Extract estimation window data
  est_data <- merged[est_start_pos:est_end_pos, ]
  
  # Run OLS regression — firm return on index return
  model <- lm(est_data[, 1] ~ est_data[, 2])
  
  # Return model
  return(model)
}
exists("estimate_market_model")

# Run market model for all firms and all events
market_models <- list()

for (event_name in names(event_dates)) {
  event_date <- event_dates[event_name]
  market_models[[event_name]] <- list()
  
  for (firm_name in names(merged_data)) {
    model <- estimate_market_model(
      merged      = merged_data[[firm_name]],
      event_date  = event_date,
      est_start   = -120,
      est_end     = -20
    )
    market_models[[event_name]][[firm_name]] <- model
  }
}

# Check it ran for all combinations
for (event_name in names(market_models)) {
  for (firm_name in names(market_models[[event_name]])) {
    model <- market_models[[event_name]][[firm_name]]
    if (!is.null(model)) {
      cat(event_name, "-", firm_name, 
          "| Alpha:", round(coef(model)[1], 6),
          "| Beta:", round(coef(model)[2], 4), "\n")
    } else {
      cat(event_name, "-", firm_name, "| NULL - check data\n")
    }
  }
}

# Save market models and merged data for use in script 04
save(market_models, merged_data, event_dates,
     file = "dissertation_models.RData")