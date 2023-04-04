
library(tidyverse)
library(R.utils)
library(lubridate)

if(!file.exists("./Proj2data")){dir.create("./Proj2data")} # Check to see if the subdirectory for storing our data exists.
# If it does not, create it.


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Set the DL link url.

download.file(fileUrl,destfile="./Proj2data/StormData.csv.bz2",method="curl") # Actually download the file.



library(R.utils)

gunzip(filename = "./Proj2data/StormData.csv.bz2", destname = "./Proj2data/StormData.csv",
                   skip = TRUE, ext = "bz2")

stormdata <- as_tibble(read.csv(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))
# Testing spead of read.csv vs fread for a huge datafile
Sys.time()
# Time: 17 seconds

Sys.time()
stormdata2 <- as_tibble(data.table::fread(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))
Sys.time()
# Time: 4 seconds

str(stormdata2)


stormdata == stormdata2

# Yep, it tracks, so for the RMD, use fread. But as_tibble because data table format gave me super weird errors that one time.

glimpse(stormdata)
str(stormdata)

max(stormdata$FATALITIES)

max(stormdata$INJURIES)

stormdata[stormdata$FATALITIES == 583,]

stormdata[stormdata$INJURIES == 1700,]

glimpse(stormdata)

# Clever: activity$date <- as.Date(activity$date)


stormdata$BGN_DATE <- as.Date(stormdata$BGN_DATE)

#Error


?lubridate

# If the dates are in the format i.e. 4/18/1950 0:00:00, I think I want the lubridate command:

stormtest <- stormdata[1:1000,]

stormtest$BGN_DATE <- mdy_hms(stormtest$BGN_DATE)

stormtest$BGN_DATE <- as.Date(stormtest$BGN_DATE)

glimpse(stormtest)

stormdata$REFNUM


Katrina <- filter(stormdata, REFNUM == "577615")

Katrina$REMARKS

# Number of fatalities = 1097


max(stormdata$FATALITIES)

max(stormdata$INJURIES)

maxfatal <- stormdata[stormdata$FATALITIES == max(stormdata$FATALITIES),]

maxfatal$REMARKS

maxhurt <- stormdata[stormdata$INJURIES == 1700,]
max
stormdata[stormdata$REFNUM == 577615,36]


# Fixing hurricane katrina's fatalities...
match("FATALITIES", colnames(stormdata))

match("INJURIES", colnames(stormdata))

stormdata[stormdata$REFNUM == 577615,23] <- 1097

stormdata[stormdata$REFNUM == 577615,23]

max(stormdata$FATALITIES)

# Okay let's stop here.
