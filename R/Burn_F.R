####################################################
# Function to correct the existing BURN_INC column in the all.data data set

Burn_F <- function(firesData, heardData) {
  firesData <- data.table::data.table(firesData)
  fire50 <- firesData[, CUM_BURN := sum(BURN), by = YEAR]
  colsKeep <- c("YEAR", "CUM_BURN")
  fire50 <- fire50[, names(fire50)[!names(fire50) %in% colsKeep] := NULL]
  fire50 <- unique(fire50)
  hData <- merge(fire50, heardData, on = "YEAR")
  hData$PROP_CUM_BURN <- (hData$CUM_BURN/100)/hData$AREA # change ha to km^2, and determine the proportion of the herd AREA
  return(hData)
}
####################################

# TODO: check 50 year time window 
# should increase from 1940-1990, then decrease because of the 50 time window.
# check out link steve sent.

# Cum sum for 1st 40 years
# then n: lengt year
# add current value - value for
