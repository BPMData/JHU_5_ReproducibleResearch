
library(tidyverse)
library(R.utils)
library(lubridate)

if(!file.exists("./Proj2data")){dir.create("./Proj2data")} # Check to see if the subdirectory for storing our data exists.
# If it does not, create it.
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Set the DL link url.


if(!file.exists("./Proj2data/Stormdata.csv.bs2")) {download.file(fileUrl,destfile="./Proj2data/StormData.csv.bz2",method="curl")} # Actually download the file.
gunzip(filename = "./Proj2data/StormData.csv.bz2", destname = "./Proj2data/StormData.csv",
       skip = TRUE, ext = "bz2")

stormdata <- as_tibble(read.csv(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Set the DL link url.

download.file(fileUrl,destfile="./Proj2data/StormData.csv.bz2",method="curl") # Actually download the file.





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

# Install cacher from a local file:

if(!file.exists("./Proj2data/cacher_1.1-2.tar.gz"))  {
      download.file("https://cran.r-project.org/src/contrib/Archive/cacher/cacher_1.1-2.tar.gz",destfile="./Proj2data/cacher_1.1-2.tar.gz",method="curl")
} else {
      install.packages("./Proj2data/cacher_1.1-2.tar.gz", repos=NULL, type= "both")
}

?install.packages

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Set the DL link url.

download.file(fileUrl,destfile="./Proj2data/StormData.csv.bz2",method="curl") # Actually download the file.


install.packages("https://cran.r-project.org/src/contrib/Archive/cacher/cacher_1.1-2.tar.gz", repos = NULL, type ="both")


install.packages("./Proj2data/cacher_1.1-2.tar.gz", repos=NULL, type= "both")

library(cacher)


# Figuring out what years are in the data set:

whatismag <- stormdata[stormdata$MAG != 0, ]
unique(whatismag$EVTYPE)

tornados <- whatismag[whatismag$EVTYPE == "TORNADO",]

?substr
substr(stormdata[1,2],1,2)

year(stormdata[1,2])


stormdata[1:10,2]

is.Date(stormdata[1,2])
as.Date(stormdata[1,2])
as.Date.POSIXlt(stormdata[1,2])
parse_date_time(stormdata[1,2],"ymd")

as.Date.POSIXlt(stormtest$BGN_DATE)

?as.Date.POSIXlt()

x <- ymd("2012-03-26")

year(x)

x <- stormdata[1,2]
x

year(x)

stormtest2 <- stormtest

stormtest2$BGN_DATE <- as.Date(stormtest2$BGN_DATE)

stormtest2[1,2]
glimpse(stormtest2)

year(stormtest2$BGN_DATE)

stormtest2$BGN_DATE <- year(stormtest2$BGN_DATE)
glimpse(stormtest2)

stormtest2$BGN_DATE <- as.Date(stormtest2$BGN_DATE, origin = "0000")


stormtest2$BGN_DATE

unique(stormtest2$BGN_DATE)
?as.Date
unlist(stormtest2[1,2])

stormdates <- stormdata[,2]

glimpse(stormdates)

stormdates$BGN_DATE <- as.Date(stormdates$BGN_DATE)

stormyears <- year(stormdates$BGN_DATE)

unique(stormyears)

# Figuring out how to calculate PROPDMG properly. PROPDMG = 25, CROPDMG = 27

match("PROPDMG", colnames(stormdata))
match("CROPDMG", colnames(stormdata))

range(stormdata$PROPDMG)

summary(stormdata$PROPDMG)

hist(stormdata$PROPDMG)

?hist

hist(stormdata$PROPDMG, xlim = "1000")

?hist

lowpropdamage <- stormdata[stormdata$PROPDMG %in% c(1:1000),]

hist(lowpropdamage$PROPDMG)



stormtest$EVTYPE <- tolower(stormtest$EVTYPE)


stormdata2 <- stormdata

glimpse(stormdata2)

stormdata2$EVTYPE <- tolower(stormdata2$EVTYPE)

fullgroups <- group_by(stormdata2, EVTYPE)

unique(stormdata2$EVTYPE)

stormhealth <- filter(fullgroups,FATALITIES > 0 | INJURIES > 0) %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
      arrange(desc(FATALITIES + INJURIES))

stormhealth

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

dictionary <- tolower(dictionary)

stormdata3 <- stormdata2


install.packages("stringdist")
library(stringdist)

stormdata3$EVTYPE_48 <- dictionary[amatch(stormdata3$EVTYPE,dictionary,method="lv", maxDist=20)]

length(unique(stormdata2$EVTYPE))

length(unique(stormdata3$EVTYPE_48))

fullgroups48 <- group_by(stormdata3, EVTYPE_48)

stormhealth <- filter(fullgroups48,FATALITIES > 0 | INJURIES > 0) %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
      arrange(desc(FATALITIES + INJURIES))

colnames(stormhealth)

gatheredhealth <- gather(stormhealth,EVTYPE_48, VALUE, FATALITIES:INJURIES)

?gather

# No, that's stupid

rm(gatheredhealth)

# Full code for messing around to fix EVTypes for humans:

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

dictionary <- tolower(dictionary)

summed_human_events$EVTYPE

summed_human_events$EVTYPE <- tolower(summed_human_events$EVTYPE)

summed_human_events$EVTYPE_48_3 <- dictionary[amatch(summed_human_events$EVTYPE,dictionary,method="lcs", maxDist=20)]

table(summed_human_events$EVTYPE_48, summed_human_events$FATALITIES)

summed_human_events$EVTYPE[47] <- "extreme cold"
summed_human_events$EVTYPE[47]
summed_human_events$EVTYPE[67] <- "heat"
summed_human_events$EVTYPE[67]
summed_human_events$EVTYPE[217] <- "flooding"
summed_human_events$EVTYPE[217]


