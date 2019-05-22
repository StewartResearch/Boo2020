library("reproducible")
Checksums(path = file.path(getwd(), "data"))
filesDownloaded <- preProcess(url = "https://drive.google.com/open?id=18W7qiSgJI4kBRMX4-KVeHTLp5csVGtle", 
           destinationPath = file.path(getwd(), "data"), archive = "plotObjects.zip")
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
  names(dt) <- paste0("Year", 1819:2057) # Plot should be from 1837
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
dataName <- file.path(getwd(), "data", "lambdaTableAllScenarios.rds")
saveRDS(object = lambdaTable, file = dataName)
write.csv(x = lambdaTable, file =  file.path(getwd(), "data", "lambdaTableAllScenarios.csv"))
googledrive::drive_upload(media = dataName, path = googledrive::as_id("1EuchcNr0ZRH4GkIV4gaChZtKIkvAkrcT"))
