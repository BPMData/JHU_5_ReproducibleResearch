# I completely broke imputing the missing data somehow, let's start again.

actdata <- read.csv("repdata_data_activity/activity.csv")

group_by(actdata, date) %>%
      summarize(DailyStepCount = sum(steps)) %>%
      replace_na(list(date = 0, DailyStepCount = 0)) -> CleanDailySteps

NoNAsDailySteps <- replace_na(DailySteps, list(date = 0, DailyStepCount = 0))

sapply(actdata,function(x) sum(is.na(x)))

sum(unlist(lapply(actdata,function(x) sum(is.na(x)))))

sum(sapply(actdata,function(x) sum(is.na(x))))
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

max(groupedintervals$TotSteps)


# Okay, so that's right, #ChatGPT verified... so the mistake is here, maybe?


groupedintervals %>%
      mutate(AvgSteps = TotSteps/61) -> intervalavgs

max(intervalavgs$AvgSteps)
max(intervalavgs$TotSteps)

sum(CleanActData$steps)


sum(intervalavgs$AvgSteps)*61

sum(is.na(actdata[1]))


# Now let's calculate imputed properly

imputed <- actdata

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
            imputed[i,1] <- intervalavgs[intervalavgs$interval == actdata[i,3],3]
      }
}

imputed2 <- actdata

for (i in 1:17568) {
      if ( is.na(actdata[i,1]) == TRUE) {
            imputed[i,1] <- interval_avgs[interval_avgs$interval == actdata[i,3],3]
      }
}


max(imputed$steps, na.rm = TRUE)
max(actdata$steps, na.rm = TRUE)
max(CleanActData$steps)
max(intervalavgs$AvgSteps)
max(intervalavgs$TotSteps)


# IT CAME OUT TOTALLY WRONG AGAIN. WHY???

# Oh, it's because I had written == actdata[i,3], 2] when it needed to be actdata[i,3],3]. Easy fix.

# Okay, I think imputed is right again.  Now we need DailySteps from Imputed...


group_by(imputed, date) %>%
      summarize(DailyStepCount = sum(steps)) -> ImputedDailySteps

max(DailySteps$DailyStepCount)
mean(DailySteps$DailyStepCount)
max(ImputedDailySteps$DailyStepCount)
mean(DailySteps$DailyStepCount)






#Now we can try another histogram...



ggplot(ImputedDailySteps, aes(x=DailyStepCount)) +
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


# OG histogram without imputed data

group_by(CleanActData, date) %>%
      summarize(DailyStepCount = sum(steps)) -> DailySteps


ggplot(DailySteps, aes(x=DailyStepCount)) +
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

summary(DailySteps)
summary(ImputedDailySteps)

# They look really different, which makes sense the Imputed one would have way fewer 0 days.

# Creating a weekdays, Weekends dummy variable and binding it to Imputed....

imputed_dates <- imputed

str(imputed_dates)

for (i in 1:17568) {
      imputed_dates[i,2] <- as.Date.character(imputed[i,2])
      }

# Screw this for loop, do it in dplyr

imputed_dates <- imputed_dates %>%
      mutate_at(2,as.Date.character)


imputed_dates[700,2]

# That fixed it immediately.

weekdays(imputed_dates[300,2])

# I'm gonna have to remember how to make dates... look at some old swirl lessons...

# Let's create our weekdays vector

daysoftheweek <- weekdays(imputed_dates[[2]])




weekdays <-  daysoftheweek %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")

weekdays

type

sum(weekdays)

nrow(imputed_dates)

sum(weekdays)/nrow(imputed_dates)

5/7

# Looks about right.

weekends <- daysoftheweek %in% c("Saturday","Sunday")
weekends
sum(weekends)


daysfactor <- ifelse(weekdays, "weekday", "weekend")


daysfactor <- as.factor(daysfactor)


length(daysfactor)

imputeddays <- cbind(imputed, daysfactor)

sum(weekends) + sum(weekdays)
nrow(imputed_dates)

?rbind

days <- as_tibble(t(rbind(weekdays,weekends)))

sum(as.integer(weekdays))

str(weekends)
str(weekdays)
str(days)


days[1,1]
as.integer(days[1,1])

x <- c(1:7)
y <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
z <- letters[1:7]

testframe <- as_tibble(cbind(x,y,z))


glimpse(testframe)

testframe %>%
      mutate_at(2, as.factor) -> testframe


glimpse(testframe)

levels(testframe$y)

as.integer(days[2,2])
days[2,2]

sapply(days, as.integer) -> dummies


imputed_daysoftheweek <- cbind(imputed_dates,dummies)

imputed_daysoftheweek %>%
      mutate(across(c(4,5), as.factor)) -> imputed_daysoftheweek

str(imputed_daysoftheweek)

?across

imputed_daysoftheweek %>%
      filter(weekdays == 1)  -> only_weekdays
imputed_daysoftheweek %>%
      filter(weekends == 1)  -> only_weekends

str(only_weekends)

nrow(only_weekdays)+nrow(only_weekends)

ggplot(groupedintervalsavg,aes(x = WeeklySteps, y = interval)) + geom_smooth()

ggplot(only_weekdays, aes(x = interval, y = steps)) + geom_smooth() + labs(title = "Weekday Step Pattern")

ggplot(only_weekends, aes(x = interval, y = steps)) + geom_smooth() + labs(title = "Weekend Step Pattern")


# Do we have to group by interval?

# Using our data with an actual factor vector:

imputed_dates %>% filter(day_type == "weekday") %>%
      select(steps, interval) -> imputed_weekdays



imputed_dates %>% filter(day_type == "weekend") %>%
      select(steps, interval) -> imputed_weekends


nrow(imputed_weekdays) + nrow(imputed_weekends) == nrow(actdata)

####

only_weekdays %>%  select(steps, interval) %>%
      group_by(interval) %>%
      summarize(TotSteps = round(sum(steps))) -> weekdays_groupedintervals

only_weekends %>%  select(steps, interval) %>%
      group_by(interval) %>%
      summarize(TotSteps = round(sum(steps))) -> weekends_groupedintervals


weekdays_groupedintervals %>%
      mutate(AvgSteps = TotSteps/61, Weekday = 1) -> weekdays_avgs

weekends_groupedintervals %>%
      mutate(AvgSteps = TotSteps/61, Weekday = 0) -> weekends_avgs

fullweekavgs <- cbind(weekdays_avgs,weekends_avgs)

ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) + geom_smooth() + labs(title = "Weekday Step Pattern, Averaged, Smooth")

ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) + geom_smooth() + labs(title = "Weekend Step Pattern, Averaged, Smooth")


ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) + geom_line() + labs(title = "Weekday Step Pattern, Averaged, Line")

ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) + geom_line() + labs(title = "Weekend Step Pattern, Averaged, Line")


ggplot()

p1 <- ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) +
      geom_line() +
      labs(title = "Weekday Step Pattern, Averaged, Line")

p2 <- ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) +
      geom_line() +
      labs(title = "Weekend Step Pattern, Averaged, Line")

gridExtra::grid.arrange(p1, p2)

p3 <- ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.5) +
      labs(title = "Weekday Step Pattern, Averaged, Line")

p4 <- ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.5) +
      labs(title = "Weekend Step Pattern, Averaged, Line")

gridExtra::grid.arrange(p3, p4)

p5 <- ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.25) +
      labs(title = "Weekday Step Pattern, Averaged, Line")

p6 <- ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.25) +
      labs(title = "Weekend Step Pattern, Averaged, Line")

gridExtra::grid.arrange(p5, p6)

px <- ggplot(weekdays_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.125) +
      labs(title = "Weekday Step Pattern, Averaged, Line")

py<- ggplot(weekends_avgs, aes(x = interval, y = AvgSteps)) +
      geom_smooth(span = 0.125) +
      labs(title = "Weekend Step Pattern, Averaged, Line")

gridExtra::grid.arrange(px, py)



### Graph for the RMD file

ggplot(interval_avgs, aes(x = interval, y = AvgSteps)) +
      geom_line(color = "red2", linetype = 1) +
      labs(title = "Average Steps Taken During Each \nFive Minute Interval Across All Days", y = "Average STeps Taken", x = "Time of 5-Minute Interval During the Day") +
      theme(panel.background = element_rect(fill = "lavenderblush1"),
            plot.background = element_rect(fill = "lavenderblush1"),
            panel.ontop = FALSE) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.grid.major.x = element_line(color = "lightsteelblue4"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightsteelblue4",
                                              size = 0.75,
                                              linetype = 2),
            panel.grid.minor.y = element_blank())

max(interval_avgs$AvgSteps)

interval_avgs[interval_avgs$AvgSteps == max(interval_avgs$AvgSteps),1]

sapply(actdata,function(x) sum(is.na(x)))

sum(sapply(actdata,function(x) sum(is.na(x))))

class(sum(sapply(actdata,function(x) sum(is.na(x)))))

nrow(actdata)

sum(sapply(actdata,function(x) sum(is.na(x)))) / nrow(actdata)

sum(sapply(actdata,function(x) sum(is.na(x)))) / nrow(actdata) * 100


# Sizing option tests

# Using aspect.ratio in theme()

ggplot(ImputedDailySteps, aes(x = DailyStepCount)) +
      geom_histogram(bins = 20, fill = "navajowhite", color = "midnightblue") +
      labs(title = "Histogram of Daily Step Counts Using Imputed Data, 20 Bins", y = "Count", x = "Total Steps Taken / Day") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.background = element_rect(fill = "lightskyblue1"), plot.background = element_rect(fill = "lightskyblue1"), panel.ontop = FALSE, aspect.ratio = .75) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(color = "red4", size = 0.75, linetype = 2), panel.grid.minor.y = element_line(color = "red4", size = 0.25, linetype = 2))

# Using options()

ggplot(CleanDailySteps, aes(x=DailyStepCount)) +
      geom_histogram(bins = 20, fill = "navajowhite", color = "midnightblue") +
      labs(title = "Histogram of Daily Step Counts Using Cleaned Data, 20 Bins", y = "Count", x = "Total Steps Taken / Day") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.background = element_rect(fill = "lightskyblue1"),
            plot.background = element_rect(fill = "lightskyblue1"),
            panel.ontop = FALSE) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "red4",
                                              size = 0.75,
                                              linetype = 2),
            panel.grid.minor.y = element_line(color = "red4",
                                              size = 0.25,
                                              linetype = 2)) +

      ?option2
