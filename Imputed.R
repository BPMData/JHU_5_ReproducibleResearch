# I completely broke imputing the missing data somehow, let's start again.

actdata <- read.csv("repdata_data_activity/activity.csv")

group_by(actdata, date) %>%
      summarize(DailyStepCount = sum(steps)) -> DailySteps

NoNAsDailySteps <- replace_na(DailySteps, list(date = 0, DailyStepCount = 0))

lapply(actdata,function(x) sum(is.na(x)))

CleanActData <- replace_na(actdata,list(steps = 0, date = 0, interval = 0 ))

lapply(CleanActData,function(x) sum(is.na(x)))

CleanActData %>%  select(steps, interval) %>%
      group_by(interval) %>%
      summarize(WeeklySteps = sum(steps)) -> groupedintervals

groupedintervals %>%
      mutate(WeeklySteps = round(WeeklySteps/7)) -> avg

avg[avg$WeeklySteps == max(avg$WeeklySteps),]

actOG <- actdata

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
            actdata[i,1] <- avg[avg$interval == actdata[i,3],2]
      }
}

#   Experimenting
# actdata[9000,1]
# actdata[9000,3]
#
#
# avg[avg$interval == actdata[9000,3], ]
#
#
# avg[avg$interval == actdata[9000,3], 2]
#
# actdata[9000,1] <- avg[avg$interval == actdata[9000,3], 2]
#
# actdata[9000,1]

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
            actdata[i,1] <- avg[avg$interval == actdata[i,3],2]
      }
}

max(actdata$steps)

sum(is.na(actdata[1]))

# Looks like it worked....

imputed <- actdata

max(imputed$steps)

str(imputed)

# Hey, steps is a number like it should be, no need to mutate anything. What did I do last time?

group_by(imputed, date) %>%
      summarize(DailyStepCount = sum(steps)) -> ImputedDailySteps

# The error comes from ImputedDailySteps, not anything else...

sum(imputed$steps[imputed$date == "2012-10-01"])

imputed$date

# I think I've been calculating avg wrong this entire time...


uniquedays <- unique(actdata$date)

length(uniquedays) #61

max(actdata$steps)

max(CleanActData$steps)


OGsteps <- actdata$steps

max(OGsteps, na.rm = TRUE) # It's 806. Where the HELL was 1561 coming from?


# Calculate # of steps per day...

group_by(CleanActData, date) %>%
      summarize(DailyStepCount = sum(steps)) -> DailySteps

sum(CleanActData$steps)
sum(DailySteps$DailyStepCount)

# Mean and median of DailySteps

mean(DailySteps$DailyStepCount)
median(DailySteps$DailyStepCount)

summary(DailySteps)

# I all can think of is the NAs resulted in some wonky stuff.

# Calculate AVG properly...

CleanActData %>%  select(steps, interval) %>%
      group_by(interval) %>%
      summarize(TotSteps = sum(steps)) -> groupedintervals

# Okay, so that's right, #ChatGPT verified... so the mistake is here, maybe?


groupedintervals %>%
      mutate(AvgSteps = TotSteps/61) -> intervalavgs

sum(intervalavgs$AvgSteps)*61

sum(CleanActData$steps)

sum(is.na(actdata[1]))

# Now let's calculate imputed properly

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
            actdata[i,1] <- intervalavgs[intervalavgs$interval == actdata[i,3],2]
      }
}
