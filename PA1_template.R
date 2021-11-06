library(dplyr)
library(ggplot2)
library(tidyr)

#1. Code for reading in the dataset and/or processing the data

unzip(zipfile="repdata_data_activity.zip")

activity <- read.csv("activity.csv", header = TRUE)

##Determining the NAs
colSums(is.na(activity))
#There are 2304 NAs in steps column and 0 to date and interval columns
NAs <- which(is.na(activity))
#The indexes of all NAs in steps column

Noah <- activity[NAs,]
#Table with only NAs in steps. We can see here that there is a pattern for all NAs.

#2, What is Mean Total Number of Steps Taken Per Day?

activity_by_day <- group_by(activity, date) %>% 
        summarize(total_steps_per_day = sum(steps))

colSums(is.na(activity_by_day))
#There are 8 rows with NAs

# Histogram about total steps taken per day

hist(activity_by_day$total_steps_per_day,xlab = "Total Steps per Day",main = "Total Steps per Day", breaks = 20)

# Mean and Median of total steps taken per day.
mean(activity_by_day$total_steps_per_day, na.rm = TRUE)
#10766.19
median(activity_by_day$total_steps_per_day, na.rm = TRUE)
#10765 

#3. What is the Average Daily Activity Pattern?

activity_by_interval <- group_by(activity, interval) %>%
        summarize(average_steps_per_interval = mean(steps, na.rm = TRUE))

ggplot(data = activity_by_interval, mapping = aes(x = interval, y = average_steps_per_interval)) +
        geom_line() +
        ylab("Average Steps Per Interval") +
        xlab("Interval")

max <- arrange(activity_by_interval, desc(average_steps_per_interval))
max[1,]
#The 835 interval has the most number of steps on average across all the days in the dataset.

#4. Imputing Missing Values

nrow(Noah)
# We get 2304 rows consisting of NAs

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_activity <- activity%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(mean_activity)

mean_activity_by_day <- group_by(mean_activity, date) %>% 
        summarize(total_steps_per_day = sum(steps))

hist(mean_activity_by_day$total_steps_per_day,xlab = "Total Steps per Day",main = "Total Steps per Day", breaks = 20)

mean(mean_activity_by_day$total_steps_per_day, na.rm = TRUE)
# 10766.19
median(mean_activity_by_day$total_steps_per_day, na.rm = TRUE)
# 10766.19

#5. Are there Differences in Activity Patterns between Weekdays and Weekends?

mean_activity$date <- as.Date(mean_activity$date)

mean_activity$weekday <- weekdays(mean_activity$date)
mean_activity$weekend <- ifelse(mean_activity$weekday=="Saturday" | mean_activity$weekday=="Sunday", "Weekend", "Weekday")

mean_activity_by_interval <- mean_activity %>% 
        group_by(interval, weekend) %>%
        summarize(average_steps_per_interval = mean(steps, na.rm = TRUE))

ggplot(data = mean_activity_by_interval) +
        geom_line(mapping = aes(x = interval, y = average_steps_per_interval)) +
        facet_wrap(~weekend, nrow = 2) +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps per Interval")