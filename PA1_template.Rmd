---
title: 'Reproducible Research: Peer Assignment 1'
author: "PB"
date: "5 January 2017"
output: html_document
---

**Loading and processing the data**

```{r, results='asis'}
# Load the  necessary packages

library(dplyr)
library(ggplot2)

# Read and clean the data

data<-read.csv("activity.csv")
data_clean<-data[!is.na(data$steps),]
head(data_clean,5)
```

**What is the mean total number of steps taken per day? **

For this part of the assignment, you can ignore the missing values in the dataset

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate the mean and median of the total number of steps taken per day

```{r, results='asis'}
# Group the data by day

daily <- group_by(data_clean, date)
total_steps_by_day <- summarize(daily, total=sum(steps))

# Plot the histogram

hist(total_steps_by_day$total, main="Histogram of total number of steps per day", xlab="Total number of steps in a day")

# Get summary data 

summary(total_steps_by_day)
summarize(total_steps_by_day, mean=mean(total_steps_by_day$total), median=median(total_steps_by_day$total))
```

**Answer: The mean is 10766 and the median is 10765.**


**What is the average daily activity pattern?**

1. Make a time series plot(i.e. **type="l"**) of the 5-minute inteval (x-axis) and the average number of steps taken averaged across all day (y-axis)
2. Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?

```{r, results='asis'}
steps_in_interval<- aggregate(steps ~ interval, data_clean, mean)

# Draw a time series plot

 with(steps_in_interval,(plot(interval, steps, type="l", main="Average number of steps over all days",xlab="Inteval", ylab="Average number of steps")))
 
# Find the row with the max number of steps
 
max_row <- which.max(steps_in_interval$steps)

# Find the inverval with this max

steps_in_interval[max_row, ]
```

**Answer: Interval 835 has the maximum average value of steps** 

**Inputting missing values**

There are a number of day/intervals values in the dataset (coded as **NA**). The presence of missing day may introduce bias into some calculations or summaries of the data. 

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and the median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of inputing missing data on the estimates of the total daily number of steps?

**NA Strategy: The missing data strategy that I will use is to substitute average daily steps for the missing data.**

```{r, results='asis'}
mean_steps<-summarize(daily, mean=mean(steps))

# Intialise the data to the orginal data

data_filled<-data

# Fill NAs in with mean data

for(i in 1:nrow(data_filled)){if(is.na(data_filled$steps[i]))        data_filled$steps[i]<-mean_steps$mean[data_filled$date[i]]}

# Now reprocess the data with the filled values

daily_filled <- group_by(data_filled, date)
total_steps_by_day <- summarize(daily_filled, total=sum(steps))

# Draw the new histogram 

hist(total_steps_by_day$total, main="Histogram of total number of steps per day", xlab="Total number of steps in a day (filled data)")

# Get summary statistics for comparison

summary(total_steps_by_day)

```

**Answer: The new mean is smaller than the old one (10587 versus 10766) and the new median is slightly smaller than the old one (10600 versus 10765).**

**Are there differences in activity patterns between weekdays and weekends?**

For this part the **weekdays()** function may be of some help. Use the dataset with the filled-in missing values  for this part.

1. Create a **new factor variable** in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or a weekend day.

2. Make a panel plot containing a time series plot (i.e. **type="l"**) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays or weekend days (y-axis). 

```{r, results='asis'}
# Create the new factor using the filled data
data_filled['day_type']<-weekdays(as.Date(data_filled$date))
data_filled$day_type[data_filled$day_type %in% c('Saturday', 'Sunday')]<-"weekend"
data_filled$day_type[data_filled$day_type != "weekend"]<-"weekday"
data_filled$day_type<-as.factor(data_filled$day_type)
head(data_filled,5)

# Recalculate steps in interval using filled data and day type

filled_steps_in_interval<-aggregate(steps ~ interval + day_type, data_filled, mean)

# Create a comparison plot for weekdays and weekends

ggplot(filled_steps_in_interval, aes(interval, steps))+geom_line()+facet_grid(day_type ~.) + xlab("5-minute interval") + ylab("Number of steps")
```




