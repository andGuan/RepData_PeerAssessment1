---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
data <- read.csv("/Users/will/test/repdata/activity.csv", header = TRUE)
data$date <- as.Date(data$date)
total_steps_per_day <- aggregate(steps ~ date, data, sum)
hist(total_steps_per_day$steps, 
     main = "Histogram of the total number of steps taken each day", 
     xlab = "Total Steps per a day", 
     col = "lightblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## What is mean total number of steps taken per day?

``` r
mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)
cat("Mean number of steps taken each day: ", mean_steps, "\n")
```

```
## Mean number of steps taken each day:  10766.19
```

``` r
cat("Median number of steps taken each day", median_steps, "\n")
```

```
## Median number of steps taken each day 10765
```

## What is the average daily activity pattern?

``` r
avg_steps_per_interval <- aggregate(steps ~ interval, data, mean)
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, 
     type = "l", 
     main = "Time series plot of the average number of steps taken", 
     xlab = "5-minute interval", 
     ylab = "averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
max_interval <- avg_steps_per_interval$interval[which.max(avg_steps_per_interval$steps)]
cat("the maximum number of steps in 5-minute interval", max_interval, "\n")
```

```
## the maximum number of steps in 5-minute interval 835
```

## Imputing missing values

``` r
missing_values <- sum(is.na(data$steps))
cat("The number of Missing data: ", missing_values, "\n")
```

```
## The number of Missing data:  2304
```

``` r
filled_data <- data
for (i in 1:nrow(filled_data)) {
  if (is.na(filled_data$steps[i])) {
    interval_index <- which(avg_steps_per_interval$interval == filled_data$interval[i])
    filled_data$steps[i] <- avg_steps_per_interval$steps[interval_index]
  }
}
total_steps_per_day_filled <- aggregate(steps ~ date, filled_data, sum)

hist(total_steps_per_day_filled$steps, 
     main = "Histogram of the total number of steps taken each day after missing values are imputed", 
     xlab = "total steps", 
     col = "lightgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
mean_steps_filled <- mean(total_steps_per_day_filled$steps)
median_steps_filled <- median(total_steps_per_day_filled$steps)
cat("mean: ", mean_steps_filled, "\n")
```

```
## mean:  10766.19
```

``` r
cat("median: ", median_steps_filled, "\n")
```

```
## median:  10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
filled_data$day_type <- ifelse(weekdays(as.Date(filled_data$date)) %in% c("Saturdat", "Sunday"), "weekends", "weekdays")
filled_data$day_type <- as.factor(filled_data$day_type)
avg_steps_per_interval_day_type <- aggregate(steps ~ interval + day_type, filled_data, mean)
library(lattice)
xyplot(steps ~ interval | day_type, data = avg_steps_per_interval_day_type, 
       type = "l", 
       layout = c(1, 2), 
       main = "the average number of steps taken per 5-minute interval across weekdays and weekends", 
       xlab = "5-minute interval", 
       ylab = "average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
