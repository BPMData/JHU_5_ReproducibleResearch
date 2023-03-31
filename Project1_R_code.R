actdata <- read.csv("repdata_data_activity/activity.csv")

# Mean and Median of Steps/Day.

library(tidyverse)

group_by(actdata, date) %>%
  summarize(DailyStepCount = sum(steps)) %>%
      summary -> stepsum

stepsum[3,2] -> Median # Median
stepsum[4,2] -> Mean # Mean

MedianNum <- substr(Median, nchar(Median) - 6 , nchar(Median))
MeanNum <- substr(Mean, nchar(Mean) - 6 , nchar(Mean))

# Histogram ####

group_by(actdata, date) %>%
      summarize(DailyStepCount = sum(steps)) -> DailySteps

NoNAsDailySteps <- replace_na(DailySteps, list(date = 0, DailyStepCount = 0))



ggplot(NoNAsDailySteps, aes(x=DailyStepCount)) +
      geom_histogram(bins = 20, fill = "navajowhite", color = "midnightblue") +
      labs(title = "Histogram of Daily Step Counts, 20 Bins", y = "Count", x = "Total Steps Taken / Day") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = NA),
            panel.ontop = TRUE) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "darkcyan",
                                              size = 0.75,
                                              linetype = 2),
            panel.grid.minor.y = element_line(color = "darkcyan",
                                              size = 0.25,
                                              linetype = 2))


# Haha okay time to stop. Next:

# Daily activity pattern graph ####

# Clearing the 0's and confirming there are only NA's in the first column
sum(is.na(actdata[,1]))
sum(is.na(actdata[,2]))
sum(is.na(actdata[,3]))

CleanActData <- replace_na(actdata,list(steps = 0, date = 0, interval = 0 ))

# Dropping the date column and grouping by interval and then taking the average
group_by(actdata, date) %>%
      summarize(DailyStepCount = sum(steps)) %>%
      summary -> stepsum


CleanActData %>%  select(steps, interval) %>%
      group_by(interval) %>%
      summarize(WeeklySteps = sum(steps)) -> groupedintervals


groupedintervals %>%
      mutate(WeeklySteps = WeeklySteps/7) -> groupedintervalsavg

ggplot(groupedintervalsavg,aes(x = WeeklySteps, y = interval)) + geom_point()

groupedintervalsweekly <- groupedintervals

max(groupedintervalsavg[,2])

max(groupedintervals[,2])


# Here's our cute plots. Smooth seems the nicest.
ggplot(groupedintervalsavg,aes(x = WeeklySteps, y = interval)) + geom_line()
ggplot(groupedintervalsavg,aes(x = WeeklySteps, y = interval)) + geom_smooth()
ggplot(groupedintervalsavg,aes(x = WeeklySteps, y = interval)) + geom_point()

avg <- groupedintervalsavg
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
# Turn our columns into vectors so we can use order()

interval <- avg$interval
WeekSteps <- avg$WeeklySteps

#Use rbind and order to create a matrix of two vectors sorted in descending by WeekSteps
# Then transpose that to make it tidy, turn the transposed matrix into a tibble,
# and take the head of this tible


rbind(interval,WeekSteps)[, order(-WeekSteps)] %>%
      t() %>%
      as.tibble() %>%
      head(1)

# Or, you know, do it properly:
groupedintervalsavg[groupedintervalsavg$WeeklySteps == 1561,]

# Or really properly

avg[avg$WeeklySteps == max(avg$WeeklySteps),]


# Imputing missing values #####

# Finding total # of missing values

sum(is.na(actdata[,1]))
sum(is.na(actdata[,2]))
sum(is.na(actdata[,3]))

# Or with lapply:

lapply(actdata,function(x) sum(is.na(x)))

# Replace the missing NAs with the average step count for that interval

# Create a vector of the steps cells we need to replace:

NAs <- is.na(actdata[1])

# Now replace them with the weekly average for that interval:
length(actdata[1])

count(actdata[1])

# Save the original dataframe so we can restore it later.
actorig <- actdata

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
      actdata[i,1] <- avg[avg$interval == actdata[i,3],2]
      }
}

# Test that it worked...

sum(is.na(actdata[1]))

# Set this to a new data frame and reset the old data frame
actdata <- actorig

 imputed <- actdata # Don't actually run this again, commenting it to make sure I don't.

 sum(is.na(imputed[1]))



# For some reason steps is character now, so let's fix that:

imputed %>%
      mutate(across(1,as.numeric)) -> imputed

# Make a histogram using imputed, but remember we need to calculate that table with days dropped again:

group_by(imputed, date) %>%
      summarize(DailyStepCount = sum(steps)) -> ImputedDailySteps


# Okay, before we go any further, I definitely fucked something up with imputed.
# Pause for now, lunch time.


# We want a histogram


ggplot(imputed, aes(x=steps)) +
      geom_histogram(bins = 20, fill = "navajowhite", color = "midnightblue") +
      labs(title = "Histogram of Daily Step Counts, 20 Bins", y = "Count", x = "Total Steps Taken / Day") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = NA),
            panel.ontop = TRUE) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "darkcyan",
                                              size = 0.75,
                                              linetype = 2),
            panel.grid.minor.y = element_line(color = "darkcyan",
                                              size = 0.25,
                                              linetype = 2))

ggplot(NoNAsDailySteps, aes(x=DailyStepCount)) +
      geom_histogram(bins = 20, fill = "navajowhite", color = "midnightblue") +
      labs(title = "Histogram of Daily Step Counts, 20 Bins", y = "Count", x = "Total Steps Taken / Day") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = NA),
            panel.ontop = TRUE) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "darkcyan",
                                              size = 0.75,
                                              linetype = 2),
            panel.grid.minor.y = element_line(color = "darkcyan",
                                              size = 0.25,
                                              linetype = 2))






actdata[1,1] <- "Kangaroo"
actdata[1,1] <- NA
is.na(actdata[1,1])
actdata[3422,1]

actdata[3422,3] # It's 2105

# What do I want? I want the value in 2nd column for the row where the value in the first column is 2105].
avg[avg$interval == 2105,2]

avg[avg$interval == actdata[3422,3],2]

2+2





































# Discarded attempts:

as.tibble(t(what2)) -> what3


rbind(interval,WeekSteps) -> what

x <- c(1,1,3:1,1:4,3)
y <- c(9,9:1)
z <- c(2,1:9)

testframe <- rbind(x,y,z)
testframe

testframe <- t(testframe)
testframe

testframe[,order(x,-y,z)]

rbind(x,y,z)[,order(x,-y,z)]

# AAAH I JUST HAD THE COMMA IN THE WRONG PLACE:

avg[, avg$WeeklySteps == max(avg$WeeklySteps)] #NO

avg[avg$WeeklySteps == max(avg$WeeklySteps),] # YES


