# Make a vector of all the years where a lambda < 1 showed showed up within the simulations

LambdaDates <- function(SimulationRun) {
  
  #SmallL <- SimulationRun < 1.0
  # rows = 300 repetitions from MCRUNS
  # columns = 239 years (1837-2076)

  extinctYear <- data.table::rbindlist(lapply(X = 1:ncol(SimulationRun), FUN = function(year){
    LambdaLess <- any(SimulationRun[,year] < 1.0)
    #extTable <- data.frame(Year = 1818 + year, Lambda = LambdaLess)
    extTable <- data.frame(Year = 1837 + year, Lambda = LambdaLess)
    return(extTable)
  }))
  return(extinctYear)
}
