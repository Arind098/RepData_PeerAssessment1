---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---
# Introduction
The following document is an assessment on the personal activity through the help of monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

Loading the data from a zip file

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")
data <- read.table(unz("activity.zip", "activity.csv"), header=TRUE, sep = ",", colClasses = c("numeric", "factor", "numeric"))
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
library(dplyr, quietly = TRUE)
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2, quietly = TRUE)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
data_per_day <- data %>% group_by(date) %>% summarize(steps_per_day = sum(steps, na.rm = TRUE))
hist1 <- ggplot(data_per_day, aes(steps_per_day)) + geom_histogram(bins = 20) + geom_vline(xintercept = mean(data_per_day$steps_per_day), size = 1.5, alpha = 0.5, col = "cyan") + xlab("Total Number of Steps Taken Each Day") +
    ylab("Count")
print(hist1)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

* Mean of the Total number of steps taken per day 9354.23
* Median of the Total number of steps taken per day 1.0395\times 10^{4}

## What is the average daily activity pattern?

1. Make a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_steps_per_interval <- data %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))
ggplot(avg_steps_per_interval, aes(x = interval, y = mean_steps)) + geom_line() +
   xlab("Time Intervals (5 Minutes)") + 
    ylab("Total Number of Steps") +
    ggtitle("Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
avg_steps_per_interval$interval[which.max(avg_steps_per_interval$mean_steps)]
```

```
## [1] 835
```

## Imputing missing values
### Counting the number of missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

### Imputing Missing values and create a new dataset

Note missing values is replaced with the mean number of steps for each interval across all of the days


```r
imputed_data <- data
missing_rows <- is.na(imputed_data$steps)
avg_steps_per_day <- with(imputed_data, tapply(steps, interval, mean, na.rm = TRUE))
imputed_data$steps[missing_rows] <- avg_steps_per_day[as.character(imputed_data$interval[missing_rows])]
head(imputed_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

### Plotting the difference between original data and the imputed data


```r
library(grid)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.4.4
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
imputed_data_per_day <- imputed_data %>% group_by(date) %>% summarize(steps_per_day = sum(steps))

hist2 <- ggplot(imputed_data_per_day, aes(steps_per_day)) + geom_histogram(bins = 20) + geom_vline(xintercept = mean(imputed_data_per_day$steps_per_day), size = 1.5, alpha = 0.5, col = "red") + xlab("Total no. of Steps Taken (Imputed Dataset)") +
    ylab("Count")
grid.arrange(hist1, hist2, ncol = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(lubridate, quietly = TRUE)
```

```
## Warning: package 'lubridate' was built under R version 3.4.4
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
imputed_data$date <- ymd(imputed_data$date)
imputed_data <- imputed_data %>% mutate(Day = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.3
```

```r
head(imputed_data)
```

```
##       steps       date interval     Day
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

2. Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
imputed_data %>% group_by(interval,Day) %>% summarise(mean_steps = mean(steps, na.rm = TRUE)) %>% ggplot(aes(x = interval, y = mean_steps, col = Day)) +    geom_line() + facet_grid(Day~.) +
    ylab("5-minute interval") +
    xlab("Average Number of Steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
