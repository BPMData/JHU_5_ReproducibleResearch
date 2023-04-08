
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
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))+
      labs(x = "Event Type", y = "Weighted Human Suffering Index", title = "10 U.S. Weather Events Which Most Affect \nHuman Health")+
      theme(legend.position = "none")

# Versus Bing's way

human10_long <- human10 %>%
      pivot_longer (cols = c(INJURIES, FATALITIES), names_to = "type", values_to = "value")

ggplot (data = human10_long) +
      geom_col (mapping = aes (x = fct_reorder(EVTYPE_MATCHED, value, .desc = TRUE), y = value, fill = type), stat = "identity") +
      theme (plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))

events <- summed_human_events$EVTYPE_MATCHED

capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s, 1, 1)),
                               {s <- substring(s, 2); if(strict) tolower(s) else s},
                               sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

summed_human_events$EVTYPE_MATCHED <- capwords(summed_human_events$EVTYPE_MATCHED)

summed_human_events$EVTYPE_MATCHED
# Maybe we should adjust for Fatalities mattering more than injuries.

human10_adj <- human10

human10_adj$FATALITIES <- human10$FATALITIES*10


human10_long_adj <- human10_adj %>%
      pivot_longer (cols = c(INJURIES, FATALITIES), names_to = "type", values_to = "value")

ggplot(data = human10_long_adj) +
      geom_col (mapping = aes (x = fct_reorder(EVTYPE_MATCHED, value, .desc = TRUE), y = value, fill = type)) +
      theme(plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      labs(x = "Event Type", y = "Weighted Human Suffering Index", title = "10 U.S. Weather Events Which Most Affect \nHuman Health")



property_events$PROPDMG_TOTAL <- case_when (
      property_events$PROPDMGEXP == "K" ~ property_events$PROPDMG * 10^3,
      property_events$PROPDMGEXP == "M" ~ property_events$PROPDMG * 10^6,
      property_events$PROPDMGEXP == "B" ~ property_events$PROPDMG * 10^9,
      TRUE ~ 0
)

property_events$CROPDMG_TOTAL <- case_when (
      property_events$CROPDMGEXP == "K" ~ property_events$CROPDMG * 10^3,
      property_events$CROPDMGEXP == "M" ~ property_events$CROPDMG * 10^6,
      property_events$CROPDMGEXP == "B" ~ property_events$CROPDMG * 10^9,
      TRUE ~ 0
)

property_events$TOTALDMG <- (property_events$PROPDMG_TOTAL + property_events$CROPDMG_TOTAL)

# We split "Hurricane/Typhoon" into 3 entries in our dictionary, "Hurricane" "Typhoon" and "Hurricane/Typhoon", and will re-collate these after the matching is done.

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")
dictionary <- tolower(dictionary)
summed_property_events$EVTYPE <- tolower(summed_property_events$EVTYPE)
summed_property_events$EVTYPE_MATCHED <- dictionary[amatch(summed_property_events$EVTYPE,dictionary,method="lcs", maxDist=60)]


# What if we didn't do that?

dictionary2 <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")
dictionary2 <- tolower(dictionary2)
summed_property_events$EVTYPE <- tolower(summed_property_events$EVTYPE)
summed_property_events$EVTYPE_MATCHED2 <- dictionary2[amatch(summed_property_events$EVTYPE,dictionary2,method="lcs", maxDist=60)]

# Yeah no let's keep it.

summed_property_events$EVTYPE_MATCHED2 <- NULL

summedptest <- summed_property_events

summedptest[summedptest$EVTYPE_MATCHED == "hurricane",] <- summedptest$EVTYPE_MATCHED

summed_property_events$EVTYPE_MATCHED %>%
      mutate(EVTYPE_MATCHED =case_when(
            EVTYPE_MATCHED == "hurricane" ~ "Hurricane/Typhoon",
            TRUE ~ EVTYPE_MATCHED
      ))

summed_property_events$EVTYPE_MATCHED %>%
      mutate(EVTYPE_MATCHED = ifelse(EVTYPE_MATCHED ==  "hurricane", "Hurricane/Typhoons",
                                     EVTYPE_MATCHED))

#ChatGPT response:

summed_property_events$EVTYPE_MATCHED <-
      summed_property_events$EVTYPE_MATCHED %>%
      case_when(
            .== "hurricane" ~ "Hurricane/Typhoon",
            TRUE ~ .
      )

typeof(summed_property_events)

is.data.frame(summed_property_events)
is_tibble(summed_property_events)

summed_property_events$EVTYPE_MATCHED <- case_when(
      summed_property_events$EVTYPE_MATCHED == "hurricane" ~ "Hurricane/Typhoon",
      summed_property_events$EVTYPE_MATCHED == "hurricane/typhoon" ~ "Hurricane/Typhoon",
      summed_property_events$EVTYPE_MATCHED == "typhoon" ~ "Hurricane/Typhoon",
      TRUE ~ summed_property_events$EVTYPE_MATCHED
      )

summed_property_events

summed_property_events$EVTYPE_MATCHED <- capwords(summed_property_events$EVTYPE_MATCHED)

summed_property_events %>%  group_by(EVTYPE_MATCHED) %>%
      summarise(TotalDamage = sum(TotalDamage)) %>%
      arrange(desc(TotalDamage)) -> summed_property_events_refined

head(summed_property_events_refined,10)

neat10 <- summed_property_events_refined[1:10,]
neat10

library(RColorBrewer)

ggplot(data = neat10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, TotalDamage, .desc = TRUE), y = TotalDamage, fill = EVTYPE_MATCHED)) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

scale_co

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) +
      geom_point(aes(color = species)) +
      scale_color_manual(values = c("plum", "salmon", "navajowhite2")) +
      facet_wrap(~species) + scale_color_brewer(direction = -1, palette = "Blues")
YlOrRd

cols <- brewer_pal(type = "seq",palette = "YlOrRD")

cols <- brewer_pal("seq",)(5)

show_col(gradient_n_pal(cols)(seq(0,1, length.out = 30)))

brewer_pal(type = "seq")(10)

show_col("YlOrRd")
gradient_n_pal("YlOrRd")

show_col(gradient_n_pal(c("yellow","orange","red"))(seq(0,1, length.out = 10)))

gradient_n_pal(c("yellow","orange","red"))(seq(0,1, length.out = 10))


ggplot(data = neat10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, TotalDamage, .desc = TRUE), y = TotalDamage/1000000000, fill = TotalDamage)) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))+
      scale_fill_gradient(low = "sienna", high = "red2")+
      theme(legend.position = "none")+
      labs(title = "The Most Economically Consequential Weather Events,\n Ordered", y = "Cumulative Damage in Billions of Dollars,\n from 1950 to 2011",
           x = "Type of Weather Event")

