# ============================================
# 02 — RETURNS CALCULATION
# ============================================

# Load dependencies
load("dissertation_working_data.RData")
library(xts)
library(zoo)

# ============================================
# CALCULATE DAILY LOG RETURNS
# ============================================

ret_AZN   <- diff(log(AZN))[-1]
ret_SAN   <- diff(log(SAN))[-1]
ret_BMY   <- diff(log(BMY))[-1]
ret_LLY   <- diff(log(LLY))[-1]
ret_DS    <- diff(log(DS))[-1]

ret_FTSE  <- diff(log(FTSE))[-1]
ret_CAC40 <- diff(log(CAC40))[-1]
ret_SP500 <- diff(log(SP500))[-1]
ret_NK225 <- diff(log(NK225))[-1]

# ============================================
# SENSE CHECKS
# ============================================

# Confirm no NAs and correct row counts
lapply(list(ret_AZN, ret_SAN, ret_BMY, ret_LLY, ret_DS,
            ret_FTSE, ret_CAC40, ret_SP500, ret_NK225),
       function(x) cat("Rows:", nrow(x), "| NAs:", 
                       sum(is.na(x)), "\n"))

# ============================================
# SAVE
# ============================================

save(ret_AZN, ret_SAN, ret_BMY, ret_LLY, ret_DS,
     ret_FTSE, ret_CAC40, ret_SP500, ret_NK225,
     file = "dissertation_returns.RData")