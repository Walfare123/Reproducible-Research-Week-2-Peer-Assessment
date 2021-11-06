# Reproducible Research: Peer Assessment 1

## 1. Loading and Preprocessing the data.
```{r, echo=TRUE}
unzip(zipfile="repdata_data_activity.zip")

activity <- read.csv("activity.csv", header = TRUE)
```

## 2. What is Mean Total Number of Steps Taken Per Day?
```{r, echo=TRUE}

library(dplyr)

activity_by_day <- group_by(activity, date) %>% 
        summarize(total_steps_per_day = sum(steps))

hist(activity_by_day$total_steps_per_day,xlab = "Total Steps per Day",main = "Total Steps per Day", breaks = 20)

mean(activity_by_day$total_steps_per_day, na.rm = TRUE)

median(activity_by_day$total_steps_per_day, na.rm = TRUE)
```

## 3. What is the Average Daily Activity Pattern?
```{r, echo=TRUE}

library(dplyr)
library(ggplot2)

activity_by_interval <- group_by(activity, interval) %>%
        summarize(average_steps_per_interval = mean(steps, na.rm = TRUE))
        
ggplot(data = activity_by_interval, mapping = aes(x = interval, y = average_steps_per_interval)) +
        geom_line() +
        ylab("Average Steps Per Interval") +
        xlab("Interval")
```
The maximum activity can be seen in the 835th interval.
```{r}
max <- arrange(activity_by_interval,desc(average_steps_per_interval))
max[1,]
```

## 4. Imputing Missing Values

I use the average of every interval to replace the NAs on each corresponding interval.

```{r, echo=TRUE}

library(dplyr)

colSums(is.na(activity))

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

mean_activity <- activity%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))

head(mean_activity)

mean_activity_by_day <- group_by(mean_activity, date) %>% 
        summarize(total_steps_per_day = sum(steps))

hist(mean_activity_by_day$total_steps_per_day,xlab = "Total Steps per Day",main = "Total Steps per Day", breaks = 20)

mean(mean_activity_by_day$total_steps_per_day, na.rm = TRUE)

median(mean_activity_by_day$total_steps_per_day, na.rm = TRUE)
```

## 5. Are there Differences in Activity Patterns between Weekdays and Weekends?

```{r, echo=TRUE}

library(dplyr)
library(ggplot2)

mean_activity$date <- as.Date(mean_activity$date)

mean_activity$weekday <- weekdays(mean_activity$date)

mean_activity$weekend <- ifelse(mean_activity$weekday=="Saturday" | mean_activity$weekday=="Sunday", "Weekend", "Weekday")

mean_activity_by_interval <- mean_activity %>% 
        group_by(interval, weekend) %>%
        summarize(average_steps_per_interval = mean(steps, na.rm = TRUE))
```

There are differences which can be seen during noon time and afternoon time.

```{r}
ggplot(data = mean_activity_by_interval) +
        geom_line(mapping = aes(x = interval, y = average_steps_per_interval)) +
        facet_wrap(~weekend, nrow = 2) +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps per Interval")
```
