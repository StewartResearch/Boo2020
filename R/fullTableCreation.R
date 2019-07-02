fullTableCreation <- function(dataPathGDrive = "https://drive.google.com/open?id=1avEapiUArQx28paSz28WlWrT69fZ7hf1", 
                          # Full path to data hosted in google drive. Should be a .zip file of all .rds files from the herds 
                          dataName = file.path(getwd(), "data", "lambdaTableAllScenarios_V3"), # full path to data to be saved without extension
                          typeOfFile = "csv", # type of file to save, currently supports: "csv" or "rds"
                          uploadToGoogleDrive = FALSE){
  library("reproducible")
  # Older version of the data: "https://drive.google.com/open?id=18W7qiSgJI4kBRMX4-KVeHTLp5csVGtle"
  dataFile <- dataPathGDrive
  Checksums(path = file.path(getwd(), "data"))
  filesDownloaded <- preProcess(url = dataFile, 
                                destinationPath = file.path(getwd(), "data"))
  filesDownloaded <- filesDownloaded$checkSums$actualFile[grepl(pattern = "rds", 
                                                                x = filesDownloaded$checkSums$actualFile)]
  filesDownloadedFull <- file.path(getwd(), "data", filesDownloaded)
  dt <- lapply(X = filesDownloadedFull, FUN = readRDS)
  
  dtClean <- lapply(X = dt, FUN = function(object){
    if (class(object) != "matrix"){
      return(object[["Lambda"]])
    } else {
      return(object)
    }
  })
  
  herdScenario <- unlist(strsplit(x = filesDownloaded, split = ".rds"))
  names(dtClean) <- herdScenario
  
  lambdaTable <- data.table::rbindlist(lapply(X = herdScenario, FUN = function(herdScen){
    dt <- data.table::data.table(dtClean[[herdScen]])
    names(dt) <- paste0("Year", 1837:2075)
    herdScenMeans <- data.table::rbindlist(lapply(names(dt), function(cl){
      data.table::data.table(
        year = sapply(strsplit(cl, split = "Year"), "[", 2 ),
        lowerCI = quantile(dt[, get(cl)], 0.05),
        mean = mean(dt[, get(cl)]),
        upperCI = quantile(dt[, get(cl)], 0.95)
      )
    }))
    herdName <- sapply(strsplit(herdScen, split = "_"), "[", 1)
    scenarioName <- sapply(strsplit(herdScen, split = "_"), "[", 2)
    herdScenMeans$herd <- herdName
    herdScenMeans$scenario <- scenarioName
    return(herdScenMeans)
  })
  )
  if (typeOfFile == "rds"){
    saveRDS(object = lambdaTable, file = paste0(dataName, ".rds"))
  } else {
    if (typeOfFile == "csv")
      write.csv(x = lambdaTable, file = paste0(dataName, ".csv"))
  }
  if (uploadToGoogleDrive)
    googledrive::drive_upload(media = dataName, path = googledrive::as_id("1LbTDB0JpcdIc_CWrRnhYAUUEJnZ0qS2Y"))
  return(lambdaTable)
}
