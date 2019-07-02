# Boo2019

Code to test the Sorensen et al. (2008) caribou recovery hypothesis with historical disturbance regimes (fire and industrial disturbance).

This is an example on how to use these module's functions:
```
fullTable <- fullTableCreation(dataPathGDrive = "https://drive.google.com/open?id=1avEapiUArQx28paSz28WlWrT69fZ7hf1", 
                              # Full path to data hosted in google drive. Should be a .zip file of all .rds files from the herds 
                              dataName = file.path(getwd(), "data", "lambdaTableAllScenarios_V3"), # full path to data to be saved without extension
                              typeOfFile = "csv") # type of file to save, currently supports: "csv" or "rds")

fullPlot <- makeCaribouPlot(dataPath = fullTable, # full data path OR R data.table object
                      startYear = 1837, # Starting year of time series for plotting
                      endYear = 2055,  # Last year of time series for plotting
                      periods = c(1937, 2007), # vector of starting years
                      plotName = file.path(getwd(), "caribouPlot"), # Full path and name of the plot file to be saved without extension
                      colorCI = c("grey70", "cornflowerblue", "brown1"),
                      colorLine = c("grey40", "blue3", "red3"),
                      plotWidth = 1000, 
                      plotHeight = 400,
                      uploadToGoogleDrive = FALSE)
```
