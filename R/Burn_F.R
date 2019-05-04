####################################################
# Function to correct the existing BURN_INC column in the all.data data set

# Tati's function - May 3, 2019
Burn_F <- function(firesData, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940) {
  # In general the firesData is the data.frame WSA$all.data object (given to F. Stewart)
  # This function can use both
  # WSA$all.data$PROP_BURN --> we *believe* is the annual proportion of caribou herd area burned
  # WSA$all.data$BURN_INC --> we *believe* is the annual total area burned
  firesData <- data.table::data.table(firesData)
  fire50 <- data.table::copy(firesData[, SUM_BURN := sum(get(colToUse)), by = YEAR])
  colsKeep <- c("YEAR", "SUM_BURN")
  fire50 <- unique(fire50[, names(fire50)[!names(fire50) %in% colsKeep] := NULL])
  fire50[, SUM_CUM := apply(X = fire50, MARGIN = 1, function(rw){
    sm <- fire50[YEAR %in% (rw[["YEAR"]]-lagYears+1):(rw[["YEAR"]]), sum(SUM_BURN, na.rm = TRUE)]}
  )]
  hdata <- merge(firesData, fire50)
  return(hdata)
}

#b <- Burn_F(firesData = herd$all.data)


###################################################################################################
# notes from old trials:

# Burn_F <- function(firesData, heardData) {
#   fire50 <- firesData[, CUM_BURN := sum(BURN), by = YEAR]
#   colsKeep <- c("YEAR", "CUM_BURN")
#   fire50 <- fire50[, names(fire50)[!names(fire50) %in% colsKeep] := NULL]
#   fire50 <- unique(fire50)
#   hData <- merge(fire50, heardData, on = "YEAR")
#   hData$PROP_CUM_BURN <- (hData$CUM_BURN/100)/hData$AREA # change ha to km^2, and determine the proportion of the herd AREA
#   #hData$PROP_CUM_BURN <- sum(hData$PROP_CUM_BURN[YEAR-50:YEAR])
#   return(hData)
# }

# burnTable <- data.table::data.table(YEAR = as.numeric(names(WSA$p50s)), BURN = WSA$p50s)
# 
# Burn_F <- function(firesData, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940) { 
#   # In general the firesData is the data.frame WSA$all.data object (given to F. Stewart)
#   # This function can use both 
#   # WSA$all.data$PROP_BURN --> we *believe* is the annual proportion of caribou herd area burned
#   # WSA$all.data$BURN_INC --> we *believe* is the annual total area burned
#   browser()
#   firesData <- data.table::data.table(firesData)
#     fire50 <- firesData[, SUM_BURN := sum(get(colToUse)), by = YEAR]
#   colsKeep <- c("YEAR", "SUM_BURN")
#   fire50 <- unique(fire50[, names(fire50)[!names(fire50) %in% colsKeep] := NULL])
#   
#   startPoint <- min(fire50$YEAR)
#   
# 
#   fire50[[paste0("CUM_BURN", lagYears)]] <- sum(fire50[YEAR-lagYears:YEAR])
#   return(hData)
# }
####################################

