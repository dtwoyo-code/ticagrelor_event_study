# ============================================
# DISSERTATION DATA ACQUISITION AND CLEANING
# Tinashe Woyo
# University of Reading
# ============================================

# Load required packages
library(quantmod)
library(zoo)

# Set date range
start <- as.Date("2006-01-01")
end   <- as.Date("2012-12-31")

# --------------------------------------------
# STEP 1 — DOWNLOAD RAW DATA FROM YAHOO FINANCE
# --------------------------------------------

# Firm share prices
getSymbols("AZN.L",  src = "yahoo", from = start, to = end)
getSymbols("SAN.PA", src = "yahoo", from = start, to = end)
getSymbols("BMY",    src = "yahoo", from = start, to = end)
getSymbols("LLY",    src = "yahoo", from = start, to = end)
getSymbols("4568.T", src = "yahoo", from = start, to = end)

# Market indices
getSymbols("^FTSE", src = "yahoo", from = start, to = end)
getSymbols("^FCHI", src = "yahoo", from = start, to = end)
getSymbols("^GSPC", src = "yahoo", from = start, to = end)
getSymbols("^N225", src = "yahoo", from = start, to = end)

# --------------------------------------------
# STEP 2 — STORE IN NAMED LIST AND SAVE RAW
# --------------------------------------------

dissertation_data <- list(
  AZN   = AZN.L,
  SAN   = SAN.PA,
  BMY   = BMY,
  LLY   = LLY,
  DS    = get("4568.T"),
  FTSE  = FTSE,
  CAC40 = FCHI,
  SP500 = GSPC,
  NIKKEI = N225
)

save(dissertation_data, file = "dissertation_raw_data.RData")

# --------------------------------------------
# STEP 3 — CHECK DATA QUALITY
# --------------------------------------------

# Check date ranges and missing values for all series
lapply(dissertation_data, function(x) {
  cat("Start:", as.character(index(x)[1]),
      "| End:", as.character(index(x)[nrow(x)]),
      "| Rows:", nrow(x),
      "| NAs:", sum(is.na(Ad(x))), "\n")
})

# Check exact NA dates for NIKKEI (highest NA count)
na_dates <- index(dissertation_data$NIKKEI)[is.na(Ad(dissertation_data$NIKKEI))]
print(na_dates)

# --------------------------------------------
# STEP 4 — HANDLE MISSING VALUES
# --------------------------------------------

# Linear interpolation for series with NAs
# NA dates confirmed as Japanese public holidays
# None fall within event windows
dissertation_data$NIKKEI <- na.approx(dissertation_data$NIKKEI)
dissertation_data$FTSE   <- na.approx(dissertation_data$FTSE)
dissertation_data$CAC40  <- na.approx(dissertation_data$CAC40)

# --------------------------------------------
# STEP 5 — EXTRACT ADJUSTED CLOSE PRICES ONLY
# --------------------------------------------

AZN   <- Ad(dissertation_data$AZN)
SAN   <- Ad(dissertation_data$SAN)
BMY   <- Ad(dissertation_data$BMY)
LLY   <- Ad(dissertation_data$LLY)
DS    <- Ad(dissertation_data$DS)
FTSE  <- Ad(dissertation_data$FTSE)
CAC40 <- Ad(dissertation_data$CAC40)
SP500 <- Ad(dissertation_data$SP500)
NK225 <- Ad(dissertation_data$NIKKEI)

# --------------------------------------------
# STEP 6 — RENAME COLUMNS CLEANLY
# --------------------------------------------

colnames(AZN)   <- "AZN"
colnames(SAN)   <- "SAN"
colnames(BMY)   <- "BMY"
colnames(LLY)   <- "LLY"
colnames(DS)    <- "DS"
colnames(FTSE)  <- "FTSE"
colnames(CAC40) <- "CAC40"
colnames(SP500) <- "SP500"
colnames(NK225) <- "NK225"

# --------------------------------------------
# STEP 7 — SAVE CLEAN WORKING DATA
# --------------------------------------------

save(AZN, SAN, BMY, LLY, DS, FTSE, CAC40, SP500, NK225,
     file = "dissertation_working_data.RData")