---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Set global environment, load libraries, load the data


``` r
knitr::opts_chunk$set(echo = TRUE)

# unzip data
unzip("activity.zip")

# load libraries and read data
library(ggplot2)
suppressMessages(library(dplyr))
activity_data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


``` r
# summarize steps per day
daily_steps <- activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

# plot histo
ggplot(daily_steps, aes(x = total_steps)) + 
        geom_histogram(binwidth = 1000, fill = "forestgreen", color = "black") +
        labs(title = "Steps per Day", x = "Steps", y = "Frequency")    
```

![](PA1_template_files/figure-html/total-1.png)<!-- -->

``` r
daily_steps_mean <- mean(daily_steps$total_steps)
daily_steps_median <- median(daily_steps$total_steps)
```

Mean: 9354.2295082

Median: 10395


## What is the average daily activity pattern?



``` r
# summarize average steps per interval
average_steps <- activity_data %>%
        group_by(interval) %>%
        summarise(ave_steps = mean(steps, na.rm = TRUE))

# plot time series line plot
ggplot(average_steps, aes(x = interval, y = ave_steps)) + 
        geom_line(color = "forestgreen") +
        labs(title = "Average Steps by Interval", 
             x = "Intervals", y = "Average steps")    
```

![](PA1_template_files/figure-html/daily-1.png)<!-- -->

``` r
# find interval with average maximum steps
max_interval <- average_steps %>%
  filter(ave_steps == max(ave_steps)) %>%
  pull(interval)
```

Interval with maximum number of steps: 835



## Imputing missing values


``` r
# count number of NAs
count_missing <- sum(is.na(activity_data$steps))
```

Number of missing values: 2304


``` r
# create sub df replacing missing values with average for interval
activity_data_sub <- activity_data %>%
  left_join(average_steps, by = "interval") %>%
  mutate(steps_sub = ifelse(is.na(steps), ave_steps, steps)) %>%
  select(-ave_steps)

# summarize steps per day in sub df
daily_steps_sub <- activity_data_sub %>%
  group_by(date) %>%
  summarise(total_steps_sub = sum(steps_sub, na.rm = TRUE))

# plot histo
ggplot(daily_steps_sub, aes(x = total_steps_sub)) + 
        geom_histogram(binwidth = 1000, fill = "forestgreen", color = "black") +
        labs(title = "Steps per Day with Sub Data", x = "Steps", y = "Frequency")    
```

![](PA1_template_files/figure-html/substitute-1.png)<!-- -->

``` r
daily_steps_mean_sub <- mean(daily_steps_sub$total_steps_sub)
daily_steps_median_sub <- median(daily_steps_sub$total_steps_sub)
```

Mean with sub data: 10766.19

Median with sub data: 10766.19


## Are there differences in activity patterns between weekdays and weekends?


``` r
# coerce date to date format
activity_data$date <- as.Date(activity_data$date)

# Create weekday/weekend factor variable
activity_data$day_type <- factor(
  ifelse(weekdays(activity_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"),
  levels = c("weekday", "weekend")
)

# Compute average steps per interval for each day type
average_steps_daytype <- activity_data %>%
  group_by(interval, day_type) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Plot time series panel
ggplot(average_steps_daytype, aes(x = interval, y = average_steps)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Average Steps per Interval by Day Type",
       x = "5-minute Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/weekends-1.png)<!-- -->
