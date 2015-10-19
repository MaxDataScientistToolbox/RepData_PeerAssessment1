# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data



```r
setwd("~/R/RepData_PeerAssessment1")
unzip("activity.zip")
activity = read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

----
Make a histogram of the total number of steps taken each day
---

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


The mean of the number of steps per interval is ` mean(steps.date$steps)`
The mean of the number of steps per interval is ` median(steps.date$steps)`


## What is the average daily activity pattern?

-----
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
-----

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

----
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
---



```r
stepsInterval[which.max(stepsInterval$steps), ]$interval
```

```
## [1] 835
```

## Imputing missing values

----
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
----


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
interval2steps <- function(interval) {
  stepsInterval[stepsInterval$interval == interval, ]$steps
}


activityFilled <- activity 


for (i in 1:nrow(activityFilled)) {
  if (is.na(activityFilled[i, ]$steps)) {
    activityFilled[i, ]$steps <- interval2steps(activityFilled[i, ]$interval)

  }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The mean of the number of steps per interval is ` mean(steps.date$steps)`
The mean of the number of steps per interval is ` median(steps.date$steps)`

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 
                              0, "weekend", "weekday")
```
# For Sunday and Saturday : weekend, Other days : weekday

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))

stepsInterval2 = aggregate(steps ~ interval + day, activityFilled, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, 
       type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
