library("dplyr")

# Define Urls and paths for the different files
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fileZip <- "exdata_data_household_power_consumption.zip"
fileOriginal <- "household_power_consumption.txt"
fileData <- "household_power_consumption_final.txt"

# Define dates used in filters and formats
stringDate1 <- "1/2/2007" 
stringDate2 <- "2/2/2007"
datesVector <- c(stringDate1, stringDate2)
stringDateFormat <- "%d/%m/%Y"
stringTimeFormat <- "%H:%M:%S"
date1 <- strptime(stringDate1, format = stringDateFormat)
date2 <- strptime(stringDate2, format = stringDateFormat)

# Removes existing previous loaded data
if (exists("hpcData")) {
  rm(hpcData)  
}

# Checks if it already has the filtered file (file with only the data that is required to generate the graphics)
if (file.exists(fileData)) {

  # Loads the filtered file
  hpcData <- read.table(fileData, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE, na.strings ="?", colClasses = c(rep("character", 2), rep("numeric", 6)))
  

} else {
  
  # Generates the filtered file for improving processing in future steps
  
  # Checks if it hasn't got the unzipped data file
  if (!file.exists(fileOriginal)) {
  
    # Checks if it hasn't got the downloaded zip file
    if (!file.exists(fileZip)) {

      # Download the zip file from its Url
      download.file(fileUrl, destfile = fileZip)

    }
    
    # Unzip the downloaded file
    unzip(fileZip, overwrite = TRUE)
  }
  
  # Gets the column names from the first row in the file
  columnNames <- read.table(fileOriginal, sep = ";", header = FALSE, nrows = 1, stringsAsFactors = FALSE, colClasses = c(rep("character", 5), "NULL", rep("character", 3)))
  
  # Initializes variables required to process the file in chunks and till it gets rows with a date greater than the ones required
  processFile <- 1
  chunkSize <- 10000
  currentRow <- 1
  
  # Processes the file till there is something available to process
  while (processFile) {
    
    # Reads from the original data file the chunk size at most
    temp <- read.table(fileOriginal, sep = ";", dec = ".", header = FALSE, skip = currentRow, nrows = chunkSize, stringsAsFactors = FALSE, na.strings ="?", colClasses = c(rep("character", 2), rep("numeric", 3), "NULL", rep("numeric", 3)))

    numRows <- nrow(temp)
    
    # Checks if there are rows to process
    if (numRows != 0) {

      currentRow <- currentRow + numRows      

      # Gets the date from the latest row in the current chunk
      lastTempDate <- strptime(temp[numRows, 1], format = stringDateFormat)

      # Checks if current chunk contains rows for the analysis
      if (as.numeric(lastTempDate - date1) >= 0) {

        # Checks if there isn't anything loaded in dataset to analyze
        if (!exists("hpcData")) {

          # Creates the dataset with the rows filtered for the dates required
          hpcData <- filter(temp, is.element(V1, datesVector))

        } else {
          
          # Adds to the existing dataset the rows filtered for the dates required
          hpcData <- bind_rows(hpcData, filter(temp, is.element(V1, datesVector)))

        }          
      }

      # Checks if the last number of rows readed doesn't match with the chunk size or the last date in the current chunk is greater than the required by the analysis
      if (numRows != chunkSize | 
            as.numeric(lastTempDate - date2) > 0) {

        # Ends the processing of the file
        processFile <- 0
      }
      
    } else {
      
        # Ends the processing of the file
        processFile <- 0
    }    
  }
  
  # Removes the temp dataset
  rm("temp")
  
  # Adds the column names to the dataset
  colnames(hpcData) <- columnNames
  
  # Removes the column names dataset
  rm("columnNames")
    
  # Writes a filtered data file with only the required information to simplify futures processing
  write.table(hpcData, fileData, sep = ";", dec = ".", na = "?", row.name = FALSE)
}

# Adds to the dataset a new variable with the Date and Time
hpcData["DateTime"] <- NA
hpcData$DateTime <- strptime(paste(hpcData$Date, hpcData$Time, sep = " "), 
                             format = paste(stringDateFormat, stringTimeFormat, sep = " "))

# Removes variables Date and Time
hpcData[1] <- NULL
hpcData[1] <- NULL


################################
#
# Creates the plot required
#
################################

png(filename = "Plot2.png", height = 480, width = 480)

Sys.setlocale("LC_TIME", "English")

plot(hpcData$DateTime, hpcData$Global_active_power, type="l", xlab = "", ylab = "Global Active Power (kilowatts)")

dev.off()
