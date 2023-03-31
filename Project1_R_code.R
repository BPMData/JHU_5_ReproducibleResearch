actdata <- read.csv("repdata_data_activity/activity.csv")

# Mean and Median of Steps/Day.

library(tidyverse)

group_by(actdata, date) %>%
  summarize(DailyStepCount = sum(steps)) %>%
      summary -> stepsum

stepsum[3,2] # Median
stepsum[4,2] # Mean

# Now for the Histogram

group_by(actdata, date) %>%
      summarize(DailyStepCount = sum(steps)) -> DailySteps

