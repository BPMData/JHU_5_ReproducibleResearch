library(tidyverse)
library(R.utils)
library(lubridate)

if(!file.exists("./Proj2data")){dir.create("./Proj2data")} # Check to see if the subdirectory for storing our data exists.
# If it does not, create it.

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Set the DL link url.

download.file(fileUrl,destfile="./Proj2data/StormData.csv.bz2",method="curl") # Actually download the file.

gunzip(filename = "./Proj2data/StormData.csv.bz2", destname = "./Proj2data/StormData.csv",
       skip = TRUE, ext = "bz2")

stormdata <- as_tibble(data.table::fread(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))

glimpse(stormdata)

stormdata$BGN_DATE <- mdy_hms(stormdata$BGN_DATE)

stormdata$BGN_DATE <- as.Date(stormdata$BGN_DATE)

glimpse(stormdata)

Katrina <- filter(stormdata, REFNUM == "577615")

Katrina$REMARKS

# Number of fatalities = 1097. Let's update the row for Katarina to reflect this.

stormdata[stormdata$REFNUM == 577615,23]

stormdata[stormdata$REFNUM == 577615,23] <- 1097

stormdata[stormdata$REFNUM == 577615,23]
max(stormdata$FATALITIES)


# Pick up from here with actual transformations.

whatismag <- stormdata[stormdata$MAG != 0, ]
