# ============================================
# 04 — EVENT STUDY: ABNORMAL RETURNS & CARs
# ============================================

# Load required objects
load("dissertation_returns.RData")
load("dissertation_working_data.RData")

# Required packages
library(xts)
library(zoo)

# Event dates — paste from 00_event_dates.R
event_dates <- as.Date(c(
  "2009-05-11",
  "2009-07-10",
  "2009-09-10",
  "2010-07-28",
  "2010-12-03",
  "2010-12-15",
  "2011-07-20"
))

names(event_dates) <- c("E1_PLATO_PR", "E2_Prasugrel_FDA",
                        "E3_PLATO_NEJM", "E4_FDA_AdCom",
                        "E5_EMA_Auth", "E6_FDA_CRL",
                        "E7_FDA_Approval")

# Function to calculate abnormal returns in event window
calc_abnormal_returns <- function(merged, event_date, model, 
                                  evt_start = -1, evt_end = 1) {
  
  all_dates  <- index(merged)
  event_pos  <- which(all_dates >= event_date)[1]
  
  evt_start_pos <- event_pos + evt_start
  evt_end_pos   <- event_pos + evt_end
  
  if (evt_start_pos < 1 | evt_end_pos > length(all_dates)) {
    message("Event window out of range")
    return(NULL)
  }
  
  # Extract event window data
  evt_data <- merged[evt_start_pos:evt_end_pos, ]
  
  # Get alpha and beta from estimated model
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  # Calculate expected returns
  expected_returns <- alpha + beta * evt_data[, 2]
  
  # Calculate abnormal returns
  abnormal_returns <- evt_data[, 1] - expected_returns
  
  return(abnormal_returns)
}
exists("calc_abnormal_returns")

# Calculate abnormal returns for all firms and events
abnormal_returns <- list()

for (event_name in names(event_dates)) {
  event_date <- event_dates[event_name]
  abnormal_returns[[event_name]] <- list()
  
  for (firm_name in names(merged_data)) {
    ar <- calc_abnormal_returns(
      merged     = merged_data[[firm_name]],
      event_date = event_date,
      model      = market_models[[event_name]][[firm_name]],
      evt_start  = -1,
      evt_end    = 1
    )
    abnormal_returns[[event_name]][[firm_name]] <- ar
  }
}

# Check results for first event
print(abnormal_returns[["E1_PLATO_PR"]])

# Calculate CARs for all firms and events
CAR_results <- list()

for (event_name in names(abnormal_returns)) {
  CAR_results[[event_name]] <- list()
  
  for (firm_name in names(abnormal_returns[[event_name]])) {
    ar <- abnormal_returns[[event_name]][[firm_name]]
    
    if (!is.null(ar)) {
      CAR_results[[event_name]][[firm_name]] <- sum(ar)
    } else {
      CAR_results[[event_name]][[firm_name]] <- NA
    }
  }
}

# Print all CARs in a clean table
cat("\nCUMULATIVE ABNORMAL RETURNS (-1, +1)\n")
cat(sprintf("%-20s %8s %8s %8s %8s %8s\n", 
            "Event", "AZN", "SAN", "BMY", "LLY", "DS"))
cat(rep("-", 65), "\n")

for (event_name in names(CAR_results)) {
  cat(sprintf("%-20s %8.4f %8.4f %8.4f %8.4f %8.4f\n",
              event_name,
              CAR_results[[event_name]]$AZN,
              CAR_results[[event_name]]$SAN,
              CAR_results[[event_name]]$BMY,
              CAR_results[[event_name]]$LLY,
              CAR_results[[event_name]]$DS))
}

# Print daily abnormal returns for all events and firms
cat("\nDAILY ABNORMAL RETURNS BY EVENT\n")

for (event_name in names(abnormal_returns)) {
  cat("\n", event_name, "\n")
  cat(sprintf("%-12s %10s %10s %10s %10s %10s\n",
              "Date", "AZN", "SAN", "BMY", "LLY", "DS"))
  cat(rep("-", 65), "\n")
  
  # Get dates from AZN (reference)
  dates <- index(abnormal_returns[[event_name]]$AZN)
  
  for (i in 1:3) {
    cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f %10.4f\n",
                as.character(dates[i]),
                as.numeric(abnormal_returns[[event_name]]$AZN[i]),
                as.numeric(abnormal_returns[[event_name]]$SAN[i]),
                as.numeric(abnormal_returns[[event_name]]$BMY[i]),
                as.numeric(abnormal_returns[[event_name]]$LLY[i]),
                as.numeric(abnormal_returns[[event_name]]$DS[i])))
  }
}
save(abnormal_returns, CAR_results, 
     file = "dissertation_results.RData")

# ============================================
# EXPLORATORY — PRE-EVENT ANTICIPATORY EFFECTS
# Not part of main analysis — for discussion only
# ============================================

load("dissertation_models.RData")
load("dissertation_returns.RData")
library(xts)
library(zoo)

# Function to get daily ARs in pre-event window
get_pre_event_ARs <- function(merged, event_date, model,
                              days_before = 10) {
  
  all_dates <- index(merged)
  event_pos <- which(all_dates >= event_date)[1]
  
  # Extract 10 days before event
  start_pos <- event_pos - days_before
  end_pos   <- event_pos - 1
  
  pre_data <- merged[start_pos:end_pos, ]
  
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  expected <- alpha + beta * pre_data[, 2]
  AR <- pre_data[, 1] - expected
  
  return(AR)
}

# E4 — FDA Advisory Committee (28 Jul 2010)
cat("\nPRE-EVENT DAILY ARs — E4 FDA Advisory Committee\n")
cat("AZN — 10 trading days before 28 July 2010\n")
cat(rep("-", 40), "\n")

E4_pre_AZN <- get_pre_event_ARs(
  merged     = merged_data$AZN,
  event_date = as.Date("2010-07-28"),
  model      = market_models$E4_FDA_AdCom$AZN,
  days_before = 10
)
print(round(E4_pre_AZN * 100, 3))

# E6 — FDA Complete Response Letter (15 Dec 2010)
cat("\nPRE-EVENT DAILY ARs — E6 FDA Complete Response Letter\n")
cat("AZN — 10 trading days before 15 December 2010\n")
cat(rep("-", 40), "\n")

E6_pre_AZN <- get_pre_event_ARs(
  merged     = merged_data$AZN,
  event_date = as.Date("2010-12-15"),
  model      = market_models$E6_FDA_CRL$AZN,
  days_before = 10
)
print(round(E6_pre_AZN * 100, 3))

# Check CHMP opinion date price window
CHMP_date <- as.Date("2010-09-23")

CHMP_AZN <- get_price_window(AZN, CHMP_date, 15)
CHMP_AZN$Firm <- "AZN"
CHMP_AZN <- normalize_to_start(CHMP_AZN)

# Print the values around the event
cat("\nAZN PRICE AROUND CHMP POSITIVE OPINION (23 Sep 2010)\n")
cat(rep("-", 45), "\n")
print(round(CHMP_AZN[13:18, c("RelDay", "NormPrice")], 3))