---
title: "Reproducible Research Project 1"
author: "Sriram Venkatachalam"
date: "Saturday, February 14, 2015"
output: html_document
---
First step is loading the data-


```r
activities <- read.csv("D:/activity.csv", header = TRUE)
```

Transformation using as.date function-


```r
activities$date <- as.Date(activities$date, format = " %Y-%m-%d")
```

Mean number of steps taken each day

```r
activities_day_wise <- aggregate(activities$steps ~ activities$date, FUN = sum)
colnames(activities_day_wise) <- c("date", "steps_taken")
# What is mean total number of steps taken per day?
hist(activities_day_wise$steps_taken, col = "blue", main = "Histogram of the total number of steps taken each day")  ###
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
avg_steps_taken_per_day <- mean(activities_day_wise$steps_taken)  ###
median_steps_taken_per_day <- median(activities_day_wise$steps_taken)  ###
```

The average daily pattern-


```r
interval_count <- data.frame(count = rep(1:288, length(unique(activities$date))))
activities_amended <- cbind(activities, interval_count)
activities_amended <- activities_amended[complete.cases(activities_amended$steps), 
    ]
avg_steps_intervalwise <- tapply(activities_amended$steps, activities_amended$count, 
    mean)
```

a) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps-


```r
max_value <- max(avg_steps_intervalwise)  ###
avg_steps_VS_time <- aggregate(activities$steps ~ activities$interval, FUN = mean)
colnames(avg_steps_VS_time) <- c("interval", "steps")
```

b) Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(ggplot2)
qplot(interval, steps, data = avg_steps_VS_time, geom = "line")  ###
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Imputing missing data with mean-

Calculate and report the total number of missing values in the dataset


```r
na_count <- nrow(activities[is.na(activities), ])
```

data set with missing values imputed with mean-


```r
copy_activities <- activities
copy_activities$steps <- impute(copy_activities$steps, mean)
```

```
## Error: could not find function "impute"
```


Histogram of the total number of steps taken each day-


```r
copy_activities_day_wise <- aggregate(copy_activities$steps ~ copy_activities$date, 
    FUN = sum)
colnames(copy_activities_day_wise) <- c("date", "steps")
hist(copy_activities_day_wise$steps, col = "red", xlim = range(copy_activities_day_wise$steps), 
    main = "Histogram of the total number of steps taken each day after imputing with mean")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

Mean and median total number of steps taken per day-


```r
avg_after_imputing <- mean(copy_activities_day_wise$steps)  ############
median_after_imputing <- median(copy_activities_day_wise$steps)
```

Inference on the new mean and medians after imputing the missing data with mean-

  It can be seen that the mean has not changed (as expected), and the median has increased by 1.18867 (a very smal value indeed)


Are there differences in activity patterns between weekdays and weekends? Let us see!


```r
activities_days <- activities
activities_days$day <- weekdays(activities_days$date)
activities_days$day_catagory <- factor(activities_days$day, levels = c("Monday", 
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), labels = c(rep("weekday", 
    5), rep("weekend", 2)))  ######
```

```
## Warning: duplicated levels in factors are deprecated
```

```r
activities_days$steps <- with(activities_days, impute(steps, mean))
```

```
## Error: could not find function "impute"
```

```r
activities_days$nbrs <- factor(activities_days$day_catagory, levels = c("weekday", 
    "weekend"), labels = 1:2)
activities_days_daywise <- aggregate(activities_days$steps ~ (activities_days$interval * 
    activities_days$nbrs), FUN = mean)
colnames(activities_days_daywise) <- c("interval", "nbrs", "steps")
```


Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
qplot(interval, steps, data = activities_days_daywise, geom = "line", facets = nbrs ~ 
    ., mtext = "plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


Yeah, there is a perceivable difference between them.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
