####################################################
# Function to correct the existing BURN_INC column in the all.data data set, as this is currently wrong.
# this number should instead be the sum of each yearly BURN50 values in the f.wsa data set.

Burn_F <- function(firesData, heardData) {
  firesData <- data.table::data.table(firesData)
  fire50 <- firesData[, CUM_BURN := sum(BURN), by = YEAR]
  colsKeep <- c("YEAR", "CUM_BURN")
  fire50 <- fire50[, names(fire50)[!names(fire50) %in% colsKeep] := NULL]
  fire50 <- unique(fire50)
  hData <- merge(fire50, heardData, on = "YEAR")
  hData$PROP_CUM_BURN <- (hData$CUM_BURN/100)/hData$AREA
  return(hData)
}
####################################



