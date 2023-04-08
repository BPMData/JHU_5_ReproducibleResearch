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

range(stormdata$BGN_DATE)

Katrina <- filter(stormdata, REFNUM == "577615")

Katrina$REMARKS
Katrina$EVTYPE

# Number of fatalities = 1097. Let's update the row for Katarina to reflect this.

stormdata[stormdata$REFNUM == 577615,23]

Katrina <- stormdata[stormdata$REFNUM == 577615,]

Katrina$REMARKS

stormdata[stormdata$REFNUM == 577615,23] <- 1097

stormdata[stormdata$REFNUM == 577615,23]
max(stormdata$FATALITIES)
stormdata[stormdata$REFNUM == 577615,23] <- 0

stormdata

maxfatal <- stormdata[stormdata$FATALITIES == max(stormdata$FATALITIES), ]
glimpse(maxfatal[c(2,7,8,23)])

glimpse(maxfatal)
which(colnames(maxfatal) == "FATALITIES")

# Pick up from here with actual transformations.

# Take only observations where someone actually died or got hurt, or that caused economic damages

damage_events <- filter(stormdata, FATALITIES > 0 | INJURIES > 0
                        | PROPDMG > 0 | CROPDMG > 0)

human_events <- filter(stormdata, FATALITIES > 0 | INJURIES > 0)
property_events <- filter(stormdata, PROPDMG > 0 | CROPDMG > 0)


# Create the pie chart
pie(data, labels = c("Weather Events Which \nHurt or Killed a Human: \n 21930",
                     "Weather Events Which \nCaused Property Damage: \n 254633"),
    main = "Comparison of Number of Weather Events \nWith Human vs Property Damage",
    col = c("red2","green3"))


stormdata$CROPDMGEXP

cropdates <- range(stormdata[stormdata$CROPDMG >0, stormdata$BGN_DATE])

crops <- filter(stormdata, CROPDMG > 0)

range(crops$BGN_DATE)
# Might not use that chart. Anyway.

# Let's focus on which events hurt humans the most:

humanhealth <- summarise(FATALITIES = sum(human_events$FATALITIES), INJURIES = sum(human_events$INJURIES))


# Nah I don't like this code:
human_events %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))%>%
      arrange(desc(FATALITIES + INJURIES))

# Oh wait you have to group_by first or it's stupid

human_events %>%  group_by(EVTYPE) %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))%>%
      arrange(desc(FATALITIES + INJURIES)) -> summed_human_events

length(unique(summed_human_events$EVTYPE))
length(unique(stormdata$EVTYPE))

# We're down to 220 EVTypes from 985, but lets reduce it to the 48 being used currently:aaaaaasdas


summed_human_events$EVTYPE_48 <- NULL
# Length 42. Much nicer. And now:

summed_human_events %>%
      arrange(desc(FATALITIES + INJURIES)) -> summed_human_events

head(summed_human_events,10) -> human10

human10



ggplot(data = human10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_48, FATALITIES, .desc = TRUE), y = INJURIES, fill = FATALITIES)) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

# Best way to present this is with a stacked bar chart, with Injuries as the base bar because it's taller.

ggplot(data = human10) +
      geom_col(mapping = aes(x = EVTYPE_48, y = INJURIES + FATALITIES)) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

# Okay, and then we can move on to property damage.

# Not what I wanted, trying again:

ggplot(data = human10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = FATALITIES, fill = "Fatalities")) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = INJURIES, fill = "Injuries", alpha = 0.5)) +
      scale_fill_manual(values = c("Fatalities" = "red", "Injuries" = "yellow")) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))



# Actually, we can't group like this until I fix PROPDMG and CROPDMG


# wtf is this bing

human10_long <- human10 %>%
      pivot_longer (cols = c (INJURIES, FATALITIES), names_to = "type", values_to = "value")

# Create stacked bar chart
ggplot (data = human10_long) +
      geom_col (mapping = aes (x = EVTYPE_48, y = value, fill = type), stat = "identity") +
      theme (plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))


# Damn it actually worked.

####
property_events <- filter(stormdata, PROPDMG > 0 | CROPDMG > 0)


cropsonly <- filter(property_events, CROPDMG > 0)

range(cropsonly$BGN_DATE)

# Huh, they only started tracking crop damage in 1993.

property_events <- filter(stormdata, PROPDMG > 0 | CROPDMG > 0)

property_events %>%  group_by(EVTYPE)

property_events_backup <- property_events


property_events <- property_events[1:10000,]
property_events_subset_compare <- property_events

## To have a look at what type of exponents we are dealing with, look at: table(data_cd0$PROPDMGEXP) and table(data_cd0$CROPDMGEXP).
## It was seen at the time of this analysis that the only values were blanks, "K"'s, "M"'s, and "B"'s.
## The following are two for loops to find the total amount of property damage per event.
## ******* These for loops take about 3 minutes each, they should be optimized somehow ****************
for (i in 1:length(data_cd0$PROPDMG)) {
      if (data_cd0$PROPDMGEXP[i] == "K") {
            data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^3
      }
      else if (data_cd0$PROPDMGEXP[i] == "M") {
            data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^6
      }
      else if (data_cd0$PROPDMGEXP[i] == "B") {
            data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^9
      }
      else data_cd0$PROPDMG_TOTAL[i] <- 0
}

## For loop to find the total amount of crop damage per event.

property_eventstest <- property_events

property_events$CROPDMG_TOTAL <- list(0)

for (i in 1:length(property_events$CROPDMG)) {
      if (property_events$CROPDMGEXP[i] == "K") {
            property_events$CROPDMG_TOTAL[i] <- property_events$CROPDMG[i]*10^3
      }
      else if (property_events$CROPDMGEXP[i] == "M") {
            property_events$CROPDMG_TOTAL[i] <- property_events$CROPDMG[i]*10^6
      }
      else if (property_events$CROPDMGEXP[i] == "B") {
            property_events$CROPDMG_TOTAL[i] <- property_events$CROPDMG[i]*10^9
      }
      else property_events$CROPDMG_TOTAL[i] <- 0
}

# Same but for property damage
property_events$PROPDMG_TOTAL <- list(0)

# This is too slow because it is indexing the data frame inside the loop over and over
for (i in 1:length(property_events$PROPDMG)) {
      if (property_events$PROPDMGEXP[i] == "K") {
            property_events$PROPDMG_TOTAL[i] <- property_events$PROPDMG[i]*10^3
      }
      else if (property_events$PROPDMGEXP[i] == "M") {
            property_events$PROPDMG_TOTAL[i] <- property_events$PROPDMG[i]*10^6
      }
      else if (property_events$PROPDMGEXP[i] == "B") {
            property_events$PROPDMG_TOTAL[i] <- property_events$PROPDMG[i]*10^9
      }
      else property_events$PROPDMG_TOTAL[i] <- 0
}

# I think it worked but is everything 25 for some reason?

unique(stormdata$PROPDMG)

table(stormdata$PROPDMG)


# Let's vectorize the operation instead. We could use a nested chain of ifelse() commands,
# but dplyr's case_when is neater:


PROPDMG_TOTAL <- numeric()

PROPDMG_TOTAL <- case_when (
      property_events$PROPDMGEXP == "K" ~ property_events$PROPDMG * 10^3,
      property_events$PROPDMGEXP == "M" ~ property_events$PROPDMG * 10^6,
      property_events$PROPDMGEXP == "B" ~ property_events$PROPDMG * 10^9,
      TRUE ~ 0
)

## Now lets find the total monetary damage, giving equal weight to property and crop damage.
data_cd0$TOTALDMG <- data_cd0$CROPDMG_TOTAL + data_cd0$PROPDMG_TOTAL

PROPDMG_TOTAL

PROPDMG_TOTAL_IFELSE <- ifelse (property_events$PROPDMGEXP == "K", property_events$PROPDMG * 10^3,
                         ifelse (property_events$PROPDMGEXP == "M", property_events$PROPDMG * 10^6,
                                 ifelse (property_events$PROPDMGEXP == "B", property_events$PROPDMG * 10^9, 0)))

# Both ran really fast. I just like the case when more, it makes more intuitive sense.


property_events$PROPDMG_TOTAL <- case_when (
      property_events$PROPDMGEXP == "K" ~ property_events$PROPDMG * 10^3,
      property_events$PROPDMGEXP == "M" ~ property_events$PROPDMG * 10^6,
      property_events$PROPDMGEXP == "B" ~ property_events$PROPDMG * 10^9,
      TRUE ~ 0
)

max(property_events$PROPDMG_TOTAL)

maxdamage <- property_events[property_events$PROPDMG_TOTAL == max(property_events$PROPDMG_TOTAL),]

maxdamage

#Same for CROPDMG
property_events$CROPDMG_TOTAL <- case_when (
      property_events$CROPDMGEXP == "K" ~ property_events$CROPDMG * 10^3,
      property_events$CROPDMGEXP == "M" ~ property_events$CROPDMG * 10^6,
      property_events$CROPDMGEXP == "B" ~ property_events$CROPDMG * 10^9,
      TRUE ~ 0
)

maxcrops <- property_events[property_events$CROPDMG_TOTAL == max(property_events$CROPDMG_TOTAL),]

maxcrops$REMARKS

maxcrops$CROPDMG_TOTAL


# Finally, create our sum:
property_events$TOTALDMG <- (property_events$PROPDMG_TOTAL + property_events$CROPDMG_TOTAL)


# And our summed totals by EVTYPE

property_events %>%  group_by(EVTYPE) %>%
      summarise(TotalDamage = sum(TOTALDMG)) %>%
      arrange(desc(TotalDamage)) -> summed_property_events

length(unique(summed_property_events$EVTYPE))

# 431 event types

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")


dictionary_hurricaneonly <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")


dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

dictionary
dictionary <- tolower(dictionary)

summed_property_events$EVTYPE <- tolower(summed_property_events$EVTYPE)


summed_property_events$EVTYPE_MATCHED <- dictionary[amatch(summed_property_events$EVTYPE,dictionary,method="lcs", maxDist=60)]

length(unique(summed_property_events$EVTYPE_MATCHED))

summed_property_events %>%
      filter(EVTYPE_MATCHED == "marine high wind")

# Trying to fix the hurricane thing

dictionary_hurricaneonly <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")



dictionary_hurricaneonly <- tolower(dictionary_hurricaneonly)

summed_property_events$EVTYPE <- tolower(summed_property_events$EVTYPE)


summed_property_events$EVTYPE_GROUPEDDIST <- dictionary_hurricaneonly[amatch(summed_property_events$EVTYPE,dictionary_hurricaneonly,method="lcs", maxDist=60)]


length(unique(summed_property_events$EVTYPE_GROUPEDDIST))
# Honestly seems pretty good
# and group again again:

summed_property_events %>%  group_by(EVTYPE_GROUPEDDIST) %>%
      summarise(TotalDamage = sum(TotalDamage)) %>%
      arrange(desc(TotalDamage)) -> summed_property_events_refined

length(unique(summed_property_events_refined$EVTYPE_GROUPEDDIST))



ggplot(data = hotel_bookings) +
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) +
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous")) +
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type 2",
           x = "Market Segment",
           y = "Total Customer Bookings",
           caption = paste0("Data from 2015 to 2017 / Legend Title Size 8, Text 6")) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


ggplot(data = summed_property_events_refined[1:10,]) +
      geom_bar(mapping = aes(y = TotalDamage, x = EVTYPE_GROUPED)) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


# I should just use col?

ggplot(data = neat10) +
      geom_col(mapping = aes(y = TotalDamage, x = EVTYPE_GROUPEDDIST, fill = EVTYPE_GROUPEDDIST)) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


neat10 <- arrange(summed_property_events_refined[1:10,], desc(TotalDamage))


library(forcats)

ggplot(data = neat10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_GROUPEDDIST, TotalDamage, .desc = TRUE), y = TotalDamage, fill = EVTYPE_GROUPEDDIST)) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

# I gotta fix this so hurricane/typhoon and hurricane aren't separate, it's bad

# Fixed it, I'm done, I'm the best

# Got my 3 figures too!
