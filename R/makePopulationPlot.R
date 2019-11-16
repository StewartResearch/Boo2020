makePopulationPlot <- function(dataPath = file.path(getwd(), "data", "lambdaTableAllScenarios_V3.csv"), # full data path OR R data.table object
                            startYear = 1845, # Starting year of time series for plotting
                            endYear = 2055,  # Last year of time series for plotting
                            periods = c(1937, 2007), # vector of starting years
                            plotName = file.path(getwd(), "caribouPlot"), # Full path and name of the plot file to be saved without extension
                            #colorCI = c("grey70", "cornflowerblue", "brown1"),
                            #colorLine = c("grey40", "blue3", "red3"),
                            colorCI = c("#f7f7f7", "#67a9cf", "#ef8a62"),
                            colorLine = c("black", "black", "black"),
                            plotWidth = 1600, 
                            plotHeight = 800,
                            uploadToGoogleDrive = TRUE){
  
  # Reg (RS)
  # -Dens (LD)
  # +Fire (MB)
  # ++Fire (HB)
  # 0Ind (NI)
  # 0Ind++Fire (NIHB)
  # 
  # And herds:
  # Cold Lake (CL)
  # Little Smoky (LS)
  # W. S. Athabasca (WSA)
  # Red Earth (RE)
  # Caribou Mountains (CM)
  
  library("ggplot2")
  library("data.table")
  if (is(dataPath, "character")){
    if (tools::file_ext(dataPath) == "csv"){
      lambdaTable <- data.table::fread(dataPath)
    } else {
      if (tools::file_ext(dataPath) == "rds"){
        lambdaTable <- readRDS(dataPath)
      } else {
        stop("File needs to be either a '.csv' or '.rds'")
      }
    }
  } else {
    if (is(dataPath, "data.table")){
      lambdaTable <- dataPath
    } else {
      stop("dataPath needs to be a path to the 'csv', to the 'rds' OR a data.table object in R") 
    }
  }
  lambdaTable <- lambdaTable[year > startYear,]
  lambdaTable <- lambdaTable[year < endYear,]
  
  # Fix factors
  lambdaTable[, c("year", "herd", "scenario") := list(as.numeric(year), factor(herd), factor(scenario))]
  herdName <- data.table(herd = c("CL", "CM", "LS", "RE", "WSA"), herdName = c("Cold Lake", "Caribou Mountains", "Little Smoky", "Red Earth", "W. S. Athabasca"))
  lambdaTable <- merge(lambdaTable, herdName)
  lambdaTable$herdName <- factor(lambdaTable$herdName, levels = c("Cold Lake", "Little Smoky", "W. S. Athabasca", "Red Earth", "Caribou Mountains"))  
  lambdaTable$scenario <- factor(lambdaTable$scenario, levels = c("Reg", "-Dens", "+Fire", "++Fire", "0Ind", "0Ind++Fire"))
  lambdaTable$period <- factor(ifelse(lambdaTable$year <= periods[1], "Period1",
                                      ifelse(lambdaTable$year >= periods[1] & 
                                               lambdaTable$year < periods[2], "Period2", 
                                             "Period3")))
  p <- ggplot() + 
    facet_grid(scenario ~ herdName) +
    geom_ribbon(data = lambdaTable,
                aes(x = year, ymin = lowerCI, ymax = upperCI, fill = period)) +
    labs(x = "Year", y = "Mean lambda") +
    scale_fill_manual(values = colorCI) +
    geom_line(data = lambdaTable[period == "Period1"],
              aes(x = year, y = mean), color = colorLine[1], size = 1.0) +
    geom_line(data = lambdaTable[period == "Period2"], 
              aes(x = year, y = mean), color = colorLine[2], size = 1.0) +
    geom_line(data = lambdaTable[period == "Period3"], 
              aes(x = year, y = mean), color = colorLine[3], size = 1.0) +
    geom_line(data = lambdaTable, aes(x = year, y = upperCI, group = 1), color = "black", size = 1) + 
    geom_line(data = lambdaTable, aes(x = year, y = lowerCI, group = 1), color = "black", size = 1) +
    geom_hline(yintercept = 1, color = 'grey60', linetype = "dashed", size = 1.0) +
    theme_bw() +
    theme(legend.position = "none")
  
  p1 <- p + theme(strip.text.x = element_text(face="bold", size = 18), strip.background.x = element_rect(fill = "white")) +
    theme(strip.text.y = element_text(face="bold", size = 18), strip.background.y = element_rect(fill = "white")) +
    theme(axis.text.x = element_text(size = 18, face = "bold")) +
    theme(axis.text.y = element_text(size = 18, face = "bold")) +
    theme(axis.title.x = element_text(size = 24, face = "bold")) +
    theme(axis.title.y = element_text(size = 24, face = "bold"))
  
  plotName <- paste0(plotName, ".png")
  png(plotName, width = plotWidth, height = plotHeight)
  print(p1)
  dev.off()
  if (uploadToGoogleDrive)
    googledrive::drive_upload(media = plotName, path = googledrive::as_id("1LbTDB0JpcdIc_CWrRnhYAUUEJnZ0qS2Y"))
  return(p1)
}
