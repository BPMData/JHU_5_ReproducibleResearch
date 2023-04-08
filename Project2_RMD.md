---
title: "Analysis of Which American Weather Events Cause the Most Damages to Human Life and Private Property, 1950 - 2011"
author: "Bryan Murphy"
date: "2023-04-04"
output:
  prettydoc::html_pretty:
    theme: hpstr
    highlight: github
    keep_md: true
---





```r
# This is our initial setup block, named setup, with include = TRUE so it will show up.  

# First we'll set any global variables we care about...
options(scipen=999) # This stops knitr from displaying 5 digit numbers as Scientific Notation.
options(cache = TRUE)
# And then we'll use a seperate code chunk to load our data.
```


```r
stormdata <- as_tibble(read.csv(file = "./Proj2data/StormData.csv", stringsAsFactors = FALSE, strip.white = TRUE))
```
<h1 align="Center"> **Part 0: Synopsis** </h1>

> Some text will go here  
> One day

<h1 align="Center"> **Part 1: Data Processing and Cleaning** </h1>

After loading our data, we look at some general descriptive statistics of the data set. We see it has 902,297 observations across 37 variables. Converting the data in the column named BGN_DATE to a format readable by R as an actual data and not a character string, we can then also see that the data begins on Jan. 3rd, 1950, and ends at Nov. 30th, 2011.

```r
stormdata$BGN_DATE <- mdy_hms(stormdata$BGN_DATE)

stormdata$BGN_DATE <- as.Date(stormdata$BGN_DATE)

range(stormdata$BGN_DATE)
```

```
## [1] "1950-01-03" "2011-11-30"
```
Out of curiosity, we decide to look up the most fatal weather incident in this dataset. A quick subsetting of the dataset by the row which has the maximum number of fatalities in the dataset tells  us that the event with the most fatalities recorded in this dataset is the 1995 Chicago heatwave, with $583$ deaths. While a tragedy, this dataset covers the year in which Hurricane Katarina happens, which should handily exceed the death toll of the 1995 Chicago Heatwave.


```r
maxfatal <- stormdata[stormdata$FATALITIES == max(stormdata$FATALITIES), ]
glimpse(maxfatal[c(2,7,8,23)])
```

```
## Rows: 1
## Columns: 4
## $ BGN_DATE   <date> 1995-07-12
## $ STATE      <chr> "IL"
## $ EVTYPE     <chr> "HEAT"
## $ FATALITIES <dbl> 583
```
However, a quick look at the Hurricane Katarina data shows us that the incident is set to $0$ fatalities, while Katrina's own *REMARKS* column indicates the event caused $1097$ fatalities! 


```r
stormdata[stormdata$REFNUM == 577615,23]
```

```
## # A tibble: 1 × 1
##   FATALITIES
##        <dbl>
## 1          0
```

```r
Katrina <- stormdata[stormdata$REFNUM == 577615,]

Katrina$REMARKS
```

```
## [1] "Hurricane Katrina was one of the strongest and most destructive hurricanes on record to impact the coast of the United States.  It will likely be recorded as one the worst natural disaster in the history of the United States to date resulting in catastrophic damage and numerous casualties in southeast Louisiana and along the Mississippi coast.  Damage and casualties resulting from Hurricane Katrina extended as far east as Alabama and the panhandle of Florida.  Katrina developed from a tropical depression southeast of the Bahamas on August 24th.  After moving through the Bahamas as a tropical storm, Katrina strengthened to a category 1 hurricane prior to landfall in south Florida around the Miami area on the 25th of August.  Katrina crossed south Florida and entered the Gulf of Mexico and began to strengthen.  Hurricane Katrina strengthened to a category 5 storm on August 28th about 250 miles south southeast of the mouth of the Mississippi River with winds reaching their peak intensity of 175 mph and a central pressure of 902 mb.  Post event analysis by the National Hurricane Center indicates that Katrina weakened slightly before making landfall as a strong category 3 storm in initial landfall in lower Plaquemines Parish. Maximum sustained winds were estimated at 110 knots or 127 mph and a central pressure of 920 mb around 610 AM CDT on August 29th in southeast Louisiana just south of Buras in Plaquemines Parish.  The storm continued on a north northeast track with the center passing about 40 miles southeast of New Orleans with a second landfall occurring near the Louisiana and Mississippi border around 945 AM CDT as a Category 3 hurricane on the Saffir Simpson scale with maximum sustained winds estimated around 105 knots or 121 mph.  Katrina continued to weaken as it moved north northeast across Mississippi during the day, but remained at hurricane strength 100 miles inland near Laurel, Mississippi.  Katrina weakened to a tropical depression near Clarksville, Tennessee on August 30th.\r\n\r\nDamage in southeast Louisiana, especially in the New Orleans area and the coastal parishes, was catastrophic.  Hurricane protection levees and floodwalls were overtopped and/or  breached resulting in widespread and deep flooding of homes and businesses.  Much of Orleans and Plaquemines Parishes and nearly all of St. Bernard Parish were flooded by storm surge. Approximately 80 percent of the city of New Orleans was flooded.  Thousands of people were stranded by the flood waters in homes and buildings and on rooftops for several days and had to be rescued by boat and helicopter. In Jefferson Parish, levees were not compromised, however many homes were flooded by either heavy rain overwhelming limited pumping capacity or storm surge water moving through in-operable pumps into the parish.  Severe storm surge damage also occurred along the north shore of Lake Pontchartrain from Mandeville to Slidell with storm surge water moving inland as far as Old Towne Slidell with water up to 6 feet deep in some locations. Hurricane force winds also caused damage to roofs, power lines, and downed trees. Windows were broken in large buildings in the metro New Orleans area from wind and wind driven debris. In areas away from storm surge flooding, wind damage was widespread with fallen trees taking a heavy toll on houses and power lines, especially over St. Tammany and Washington Parishes.  Excluding losses covered by the Federal Flood Insurance Program, insured property losses in Louisiana were estimated at 22.6 billion dollars. Overall uninsured and insured losses combined were estimated to exceed 100 billion dollars along the entire Gulf Coast.\r\n\r\nFatalities occurring in Louisiana as a result of Hurricane Katrina numbered approximately 1097 people as of late June 2006. The majority of the victims were in the New Orleans area. 480 other Louisiana residents died in other states after evacuating..\r\n Detailed information on the deaths, locations, and indirect or direct fatalities will be described in updates to Storm Data. \r\n\r\nDue to the failure of power and equipment prior to the peak of the storm, data for wind, storm surge, pressure, and rainfall are incomplete.  A university portable weather unit measured the lowest pressure of 920.2 mb near Buras around 0616 AM AM CDT on Aug 29th. with 934 mb being measured at the National Weather Service Office in Slidell at 938 AM CDT. \r\n\r\nThe highest wind gust recorded in Louisiana and the adjacent coastal waters was 99 knots (114 mph) at the Grand Isle CMAN station (338 AM CDT on August 29th) before the gage failed, though higher wind gusts certainly occurred. While most of the metro New Orleans escaped the extreme winds, the extreme eastern portions of the metro area from St Bernard Parish into extreme east New Orleans experienced the western portion of the hurricane eyewall. Wind gusts between 120 to 125 mph were recorded at a couple of locations in East New Orleans. Wind gust to hurricane force (64 kt or 74 mph) were also recorded at New Orleans Louis Armstrong Intl Airport by an FAA wind instrument. In eastern St. Tammany Parish, wind gusts to 87 knots (100) mph were measured at Slidell by a wind tower deployed by a university. An estimated wind gust of 105 kt (120 mph) was taken at a hospital in Slidell. \r\n\r\nPost storm high water surveys of the area conducted by FEMA indicated the following storm surge estimates:  Orleans Parish - 12-15 feet in east New Orleans to 9 to 12 feet along the Lakefront; St. Bernard Parish - 14 to 17 feet; Jefferson Parish - 6 to 9 feet along the lakefront to 5 to 8 feet from Lafitte to Grand Isle; Plaquemines Parish - 15 to 17 feet; St. Tammany Parish - 11 to 16 feet in southeast portion to 7 to 10 feet in western portion. All storm surge heights are still water elevations referenced to NAVD88 datum.   \r\n\r\nStorm total rainfall amounts generally ranged from 7 to 14 inches with lower amounts observed farther west toward the Atchafalaya River.  A rainfall total of 11.63 inches was measured at the National Weather Service Office in Slidell."
```
This is a baffling omission, so we'll fix that.

There might be other errors in the dataset, but it seemed very relevant to this analysis to ensure that the single most fatal weather event in the time period that dataset comprises was recorded accurately.

```r
stormdata[stormdata$REFNUM == 577615,23] <- 1097
max(stormdata$FATALITIES)
```

```
## [1] 1097
```

That accomplished, we can now begin to look at the dataset to answer the following two questions:

><center> **1: Across the United States, which types of events kill and injure the most people??**</center>  

><center> **2: Across the United States, which types of events have the greatest economic consequences?**</center>

In order to look at these questions in more detail, and to avoid the unwieldyness of dealing with an enormous dataset, we are going to subset our initial data, named *stormdata*, into two smaller datasets, *human_events*, comprising only those weather-related incidents which resulted in at least $1$ human injury and/or fatality, and *property_events*, comprising only those weather-related incidents which resulted in some measurable damage to property (including crop damage from 1993 onward).


```r
human_events <- filter(stormdata, FATALITIES > 0 | INJURIES > 0)
property_events <- filter(stormdata, PROPDMG > 0 | CROPDMG > 0)

crops <- filter(property_events, CROPDMG > 0)

range(crops$BGN_DATE)
```

```
## [1] "1993-01-04" "2011-11-28"
```
<h2 align = "center"> Weather Events Impacting Human Health </h2>

We'll start by focusing on weather events in the United States from 1950 until 2011 that had a direct impact on human health - in other words, the data contained within our subset *human_events*. 

Before we go any further, we are going to want to try to group *human_events* by the column *EVTYPE*, so we can try and look at the cumulative effect on human health for specific types of weather incidents. 

```r
human_events %>%  group_by(EVTYPE) %>%
      summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))%>%
      arrange(desc(FATALITIES + INJURIES)) -> summed_human_events
```
However, when we do this, we notice our dataset covers ***$220$*** different supposed event types. This is a lot less than the apparently ***$985$*** different type of weather events in the original data set, but according to [the NOAA itself](https://www.ncdc.noaa.gov/stormevents/details.jsp), the **Storm Events Database** should only include 48 different types of weather events! Looking at the column *EVTYPE* manually, we notice the reason there are 985 event types instead of 48 is... a massive number of typos collected over the course of over 50 years of data entry, much of which was probably done manually for decades.


```r
length(unique(summed_human_events$EVTYPE))
```

```
## [1] 220
```

```r
length(unique(stormdata$EVTYPE))
```

```
## [1] 985
```

```r
head(unique(stormdata$EVTYPE),40)
```

```
##  [1] "TORNADO"                        "TSTM WIND"                     
##  [3] "HAIL"                           "FREEZING RAIN"                 
##  [5] "SNOW"                           "ICE STORM/FLASH FLOOD"         
##  [7] "SNOW/ICE"                       "WINTER STORM"                  
##  [9] "HURRICANE OPAL/HIGH WINDS"      "THUNDERSTORM WINDS"            
## [11] "RECORD COLD"                    "HURRICANE ERIN"                
## [13] "HURRICANE OPAL"                 "HEAVY RAIN"                    
## [15] "LIGHTNING"                      "THUNDERSTORM WIND"             
## [17] "DENSE FOG"                      "RIP CURRENT"                   
## [19] "THUNDERSTORM WINS"              "FLASH FLOOD"                   
## [21] "FLASH FLOODING"                 "HIGH WINDS"                    
## [23] "FUNNEL CLOUD"                   "TORNADO F0"                    
## [25] "THUNDERSTORM WINDS LIGHTNING"   "THUNDERSTORM WINDS/HAIL"       
## [27] "HEAT"                           "WIND"                          
## [29] "LIGHTING"                       "HEAVY RAINS"                   
## [31] "LIGHTNING AND HEAVY RAIN"       "FUNNEL"                        
## [33] "WALL CLOUD"                     "FLOODING"                      
## [35] "THUNDERSTORM WINDS HAIL"        "FLOOD"                         
## [37] "COLD"                           "HEAVY RAIN/LIGHTNING"          
## [39] "FLASH FLOODING/THUNDERSTORM WI" "WALL CLOUD/FUNNEL CLOUD"
```
For example, in only the 40 entries above, we see **THUNDERSTORM WINDS** written as everything from **THUNDERSTORM WINDS** to **THUNDERSTORM WINS** to **TSTM WIND**, all of which are currently considered seperate event types, even though they obviously are not.

Before we start, we're also going to correct two egregiously unusual misentered event type observations - one event type was listed as "cold" when the official weather formatting should be **"Extreme Cold/Wind Chill"**, which will cause that observation to be mismatched as "flood" simply because flood has the letters O,L and D and is a short word, and one observation is listed as the unnecessarily loquacious "unseasonably warm and dry," which we'll amend to the official event type, **"Heat"**.

Having corrected these outliers, we'll create a vector, *dictionary*, of the NOAA's 48 official storm event types, and then use approximate string matching using the amatch() command to condense all of the 220 event types we currently have into the official 48.We are going to use the **longest common substring** method, as this will help ensure events like "Ex. Cold" get matched to the event type "Extreme Cold/Wind Chill" even though the difference in the number of characters is fairly large, rather than to something like "Funnel Cloud" or "Flood", which is closer in character count to "Ex. Cold" but don't even contain the word "cold".


```r
summed_human_events[47,1] <- "extreme cold"
summed_human_events[67,1] <- "heat"

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

dictionary <- tolower(dictionary)

summed_human_events$EVTYPE <- tolower(summed_human_events$EVTYPE)


summed_human_events$EVTYPE_MATCHED <- dictionary[amatch(summed_human_events$EVTYPE,dictionary,method="lcs", maxDist=40)]

length(unique(summed_human_events$EVTYPE_MATCHED))
```

```
## [1] 41
```
You can see we're now down to just 41 event types (because 7 of the official 48 event types never actually caused any human injuries or fatalities). Now we can look at just the top 10 entries in our refined dataset *summed_human_events* to see the 10 storm types in the United States from 1950 to 2011 which caused the most human injuries and fatalities.(We'll also delete the column containing the 220 unmatched event types)


```r
summed_human_events$EVTYPE <- NULL
head(summed_human_events,10) -> human10
human10
```

```
## # A tibble: 10 × 3
##    FATALITIES INJURIES EVTYPE_MATCHED   
##         <dbl>    <dbl> <chr>            
##  1       5633    91346 tornado          
##  2       1903     6525 excessive heat   
##  3        504     6957 strong wind      
##  4        470     6789 flood            
##  5        816     5230 lightning        
##  6        937     2100 heat             
##  7        978     1777 flash flood      
##  8       1161     1275 hurricane        
##  9         89     1975 ice storm        
## 10        133     1488 thunderstorm wind
```
Before we do anything else, we're going to be petty and recapitalize our event types, as we put them all into lower case for the sake of making our approximate matching work better.


```r
capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s, 1, 1)),
                               {s <- substring(s, 2); if(strict) tolower(s) else s},
                               sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

summed_human_events$EVTYPE_MATCHED <- capwords(summed_human_events$EVTYPE_MATCHED)
head(summed_human_events,10) -> human10
```

Now we can try actually plotting the 10 events which are most destructive to human life in the United States.

```r
ggplot(data = human10) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = FATALITIES, fill = "Fatalities")) +
      geom_col(mapping = aes(x = fct_reorder(EVTYPE_MATCHED, FATALITIES + INJURIES, .desc = TRUE), y = INJURIES, fill = "Injuries", alpha = 0.5), show.legend = FALSE) +
      scale_fill_manual(values = c("Fatalities" = "red", "Injuries" = "yellow")) +
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10)))+
      labs(x = "Event Type", y = "Weighted Human Suffering Index", title = "10 U.S. Weather Events Which Most Affect \nHuman Health", fill = "Human Health \nImpact Type")
```

![](Project2_RMD_files/figure-html/unweighted human health plot-1.png)<!-- -->
  
We see that the event which causes the most injuries by far is Tornado (the original event type which the NOAA began tracking in 1950). Next is Excessive Heat, a perennially overlooked source of human misery, suffering and death, followed by Strong Wind, Flood, and Lightning. Of particular interest are the next three event types, Heat, Flash Flood and Hurricane, because we notice their bars are almost entirely orange; this is because the graph was "stacked" by placing Injuries on top of the Fatalities bar (which was invariably smaller for all event types in the top 10), and making the Injuries bar transparent.Thus, the red Fatalities bar underneath these event types is almost the same size as the Injuries bar. In other words, Heat, Flash Flood and Hurricane don't hurt as many people as, say, Strong Wind or Flood, but they kill far more people. 

It hardly seems fair to weight Fatalities and Injuries equally, so we can call a *"Weighted Index of Human Suffering"* by assigning injuries a weight of 1 and Fatalities a higher weight. As far as I know, most of the literature on this topic presumes you know the actual nature of the injury, i.e. for the extent of calculating the actuarial payout for an insurance claim,but since the NOAA doesn't track that data, we can assign Fatality a generic weight of 10 a fatality is one order of magnitude more important than an injury.

Now, if we plot our data again, we see the following:


```r
human10_adj <- human10

human10_adj$FATALITIES <- human10$FATALITIES*10


human10_long_adj <- human10_adj %>%
      pivot_longer (cols = c(INJURIES, FATALITIES), names_to = "type", values_to = "value")

ggplot(data = human10_long_adj) +
      geom_col (mapping = aes (x = fct_reorder(EVTYPE_MATCHED, value, .desc = TRUE), y = value, fill = type)) +
      theme(plot.title = element_text (hjust = 0.5), plot.caption = element_text (hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) +
      labs(x = "Event Type", y = "Weighted Index of \nHuman Suffering", title = "10 U.S. Weather Events Which Most Affect \nHuman Health, Weighted", fill = "Human Health \nImpact Type") +
      theme(axis.text.y = element_blank())
```

![](Project2_RMD_files/figure-html/human plot fatality adjusted-1.png)<!-- -->
  
Now we see that while Tornado still leads the pack (being a storm type that tends to cause a fair number of fatalities, even though it causes far more injuries), we see that Hurricane jumps from 8th to a more plausible 4th place, Lightning and Strong Wind swapped places, which Lightning now being ranked higher, and Flash Flood is now ranked higher than Flood. All these changes make intuitive sense, which gives us reassurance that our weighting system was a good, if rudimentary idea. 

Thus, we can see that in the United States from 1950 to 2011, the weather events which most impacted human health tended were Tornadoes, heat waves, lightning storms (probably correlated somewhat with tornadoes), and Hurricanes, Strong Winds and Flash Floods. Ice Storms, as annoying as they might be to drivers in northern states, tend not to cause many fatalities, and comparatively few injuries compared to the higher ranked event types.

One final thing to note is that splitting up Excessive Heat from Heat seems to end up undercounting the effects of both events, but the NOAA itself explictly draws this distincton, so we maintained it in our analysis.

<h3 align = "center"> Weather Events with Economic Consequences </h2>

From here, we can go on to analyze weather events with economic consequences, defined as those weather events in the NOAA Storm Data set which contained an explicitly recorded value for either the variable PROPDMG or CROPDMG. PROPDMG is typically defined as damage inflicted to private property as well as public infrastructure and facilities; beginning in 1993, the NOAA began including estimates of damage to crops as well, hence the inclusion of the variable CROPDMG.

Testing `4+4` oka what
A quick calculation shows us that there are actually `stormdata %>% filter(CROPDMG > 0 & PROPDMG == 0) %>% nrow()` testing testsers

```r
2+2
```

```
## [1] 4
```

