# Make a vector of all the years where an extinction showed up within the simulations

ExtinctionDates <- function(SimulationRun) {
  # rows = 300 repetitions from MCRUNS
  # columns = 239 years (1818-2057)
  extinctYear <- data.table::rbindlist(lapply(X = 1:ncol(SimulationRun), FUN = function(year){
    didItExtinct <- any(grepl(pattern = "EXT", x = SimulationRun[,year], ignore.case = TRUE))
    extTable <- data.frame(Year = 1818 + year, EXTINCT = didItExtinct)
    return(extTable)
  }))
  return(extinctYear)
}