# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv",header=TRUE,na.strings="NA")
```

## What is mean total number of steps taken per day?

Here is a histogram of the total number of steps taken each day.

```r
total_number <- tapply(activity$steps,activity$date,sum)
hist(total_number,main="Histogram of total number of steps per day")
```

![](PA1_template_files/figure-html/histogramplot-1.png) 

Total number of steps taken per day: mean = 10766, median = 10765

## What is the average daily activity pattern?


```r
Interval <- sort(unique(activity$interval))
interval_fac <- factor(activity$interval,Interval)
activity_pattern <- tapply(activity$steps,interval_fac,function(x) mean(x,na.rm=TRUE))
```

Here is a figure showing average daily activity pattern.

```r
plot(Interval,activity_pattern,type="l",ylab="Number of steps",main="Average daily activity pattern")
```

![](PA1_template_files/figure-html/activitypatternplot-1.png) 

The 104th 5-minute-interval 835 contains the maximum number of steps.

## Imputing missing values

There are 2304 of missing values in the dataset.

Here is filling in all of the missing values in the dataset using the mean for that 5-minute interval.

```r
activity_rep <- rep(as.vector(activity_pattern),length(levels(activity$date)))
steps <- activity$steps
steps[is.na(steps)] <- activity_rep[is.na(steps)]
activity_filled <- data.frame(steps=steps,date=activity$date,interval=activity$interval)
rm("activity_rep","steps")
```

Here is a histogram of the total number of steps taken each day after filling in missing values.

```r
total_number_filled <- tapply(activity_filled$steps,activity_filled$date,sum)
hist(total_number_filled,main="Histogram of total number of steps per day after filling missing values")
```

![](PA1_template_files/figure-html/histogramplotafterfilled-1.png) 

Total number of steps taken per day: mean = 10766, median = 10766

According to the mean and median of number of steps taken per day after filling in missing values, these two measures are not affected.

## Are there differences in activity patterns between weekdays and weekends?

Split activity data into weekday and weekend.

```r
activity_filled$weekday <- weekdays(as.POSIXct(activity_filled$date))
activity_filled$weekday[activity_filled$weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "weekday"
activity_filled$weekday[activity_filled$weekday %in% c("Saturday","Sunday")] <- "weekend"
activity_filled$weekday <- factor(activity_filled$weekday,levels=c("weekday","weekend"))
```

Here is the activity pattern for weekday and weekend after filling missing values.

```r
activity_weekday <- activity_filled[activity_filled$weekday %in% "weekday",]
interval_fac <- factor(activity_weekday$interval,Interval)
activity_pattern_weekday <- tapply(activity_weekday$steps,interval_fac,function(x) mean(x,na.rm=TRUE))

activity_weekend <- activity_filled[activity_filled$weekday %in% "weekend",]
interval_fac <- factor(activity_weekend$interval,Interval)
activity_pattern_weekend <- tapply(activity_weekend$steps,interval_fac,function(x) mean(x,na.rm=TRUE))
rm("activity_weekday","activity_weekend","interval_fac")
```

Here is a figure showing average daily activity pattern for weekday and weekend.

```r
par(mfcol = c(2,1))
plot(Interval,activity_pattern_weekday,type="l",xlab="Interval",ylab="Number of steps",main="weekday")
plot(Interval,activity_pattern_weekend,type="l",xlab="Interval",ylab="Number of steps",main="weekend")
```

![](PA1_template_files/figure-html/plotactivitypatternweek-1.png) 
