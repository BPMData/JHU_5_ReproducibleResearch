
library(tidyverse)
library(R.utils)
library(lubridate)
library(forcats)
library(stringdist)

stormdata <- as_tibble(read.csv(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))

stormdata$BGN_DATE <- mdy_hms(stormdata$BGN_DATE)

stormdata$BGN_DATE <- as.Date(stormdata$BGN_DATE)

range(stormdata$BGN_DATE)

maxfatal <- stormdata[stormdata$FATALITIES == max(stormdata$FATALITIES), ]
glimpse(maxfatal[c(2,7,8,23)])

stormdata[stormdata$REFNUM == 577615,23]

Katrina <- stormdata[stormdata$REFNUM == 577615,]

Katrina$REMARKS

stormdata[stormdata$REFNUM == 577615,23] <- 1097
max(stormdata$FATALITIES)

which(colnames(stormdata) == "EVTYPE")

stormdata[stormdata$FATALITIES == max(stormdata$FATALITIES),8]


human_events <- filter(stormdata, FATALITIES > 0 | INJURIES > 0)
property_events <- filter(stormdata, PROPDMG > 0 | CROPDMG > 0)

crops <- filter(property_events, CROPDMG > 0)

range(crops$BGN_DATE)

human_events %>%  group_by(EVTYPE) %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))%>%
      arrange(desc(FATALITIES + INJURIES)) -> summed_human_events


length(unique(summed_human_events$EVTYPE))
length(unique(stormdata$EVTYPE))

head(unique(stormdata$EVTYPE),40)

summed_human_events[67,1]
summed_human_events[47,1] <- "extreme cold"
summed_human_events[67,1] <- "heat"

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

dictionary <- tolower(dictionary)

summed_human_events$EVTYPE <- tolower(summed_human_events$EVTYPE)


summed_human_events$EVTYPE_MATCHED <- dictionary[amatch(summed_human_events$EVTYPE,dictionary,method="lcs", maxDist=40)]

length(unique(summed_human_events$EVTYPE_MATCHED))


summed_human_events$EVTYPE <- NULL
head(summed_human_events,10) -> human10
human10

max(summed_human_events$FATALITIES)

ggplot(data = human10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = FATALITIES, fill = "Fatalities")) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = INJURIES, fill = "Injuries", alpha = 0.5)) +
      scale_fill_manual(values = c("Fatalities" = "red", "Injuries" = "yellow")) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))

# Versus Bing's way

human10_long <- human10 %>%
      pivot_longer (cols = c(INJURIES, FATALITIES), names_to = "type", values_to = "value")

ggplot (data = human10_long) +
      geom_col (mapping = aes (x = EVTYPE_MATCHED, y = value, fill = type), stat = "identity") +
      theme (plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))

ggplot (data = human10_long) +
      geom_col (mapping = aes (x = EVTYPE_MATCHED, y = value, fill = type), stat = "identity") +
      theme (plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))

# Maybe we should adjust for Fatalities mattering more than injuries.


