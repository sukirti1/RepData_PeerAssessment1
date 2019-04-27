---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


Loading and preprocessing the data
==================================

Loading the data


```r
data_na<- read.csv("activity.csv", na.strings = "NA", header = TRUE)
data<- data_na[!is.na(data_na),]
```




What is mean total number of steps taken per day?
================================================

Calculate the total number of steps taken per day


```r
agg<- aggregate(steps ~ date, data, sum)
```

Histogram


```r
hist(agg$steps, xlab = "steps", ylim = c(0,30), main = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

mean and median of the total number of steps taken per day


```r
mean_steps<-  mean(agg$steps)
median_steps<- median(agg$steps)
```

**mean:** 1.0766189\times 10^{4}
**median:** 10765


What is the average daily activity pattern?
===========================================

Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data_averaged<- aggregate(steps ~ interval, data, mean)
plot(data_averaged$interval, data_averaged$steps, type= "l", xlab= "5-min interval", ylab= "Average number of steps", main= "average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum_steps<- data_averaged[data_averaged$steps == max(data_averaged$steps),][1]
```


**835** is the 5-minute interval that contains maximum number of steps

Imputing missing values
=======================

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
no_of_missing_values<- nrow(data_na[is.na(data_na),])
```

**2304** :number of missing values

Devise a strategy for filling in all of the missing values in the dataset.


```r
#replacing NA’s with the mean for that 5-minute interval.
data_mean<- aggregate(steps~date, data, mean)

data_new<- data_na
for(i in 1:nrow(data_new)) {
        
        if(is.na(data_new$steps[i])){
                
                index<- which(data_averaged$interval==data_na$interval[i])
                data_new$steps[i]<- data_averaged[index,][2]
        }
}
data_new$steps<- as.numeric(data_new$steps)
```

Make a histogram of the total number of steps taken each day 


```r
agg_new<- aggregate(steps ~ date, data_new, sum)
hist(as.numeric(agg_new$steps), xlab = "steps", main = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Calculate the mean and median total number of steps taken per day.


```r
mean_steps_new<-  mean(agg_new$steps)
median_steps_new<- median(agg_new$steps)
```

**new mean:** 1.0766189\times 10^{4}
**new median** 1.0766189\times 10^{4}

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean values stays the same but there is a slight difference in meadian value.



Are there differences in activity patterns between weekdays and weekends?
========================================================================

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_new['Type of day']<- weekdays(as.POSIXct.Date(data_new$date))
data_new$`Type of day`[data_new$`Type of day` %in% c("Saturday", "Sunday")]<- "weekend"
data_new$`Type of day`[data_new$`Type of day` != "weekend"]<- "weekday"
```

Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
library(ggplot2)
data_new$`Type of day`<- as.factor(data_new$`Type of day`)
data_new_steps_by_interval <- aggregate(steps ~ interval + `Type of day`, data_new, mean)

qplot(interval, 
             steps, 
             data = data_new_steps_by_interval, 
             type = 'l', 
             geom=c("line"),
             xlab = "Interval", 
             ylab = "Number of steps", 
             main = "") +
           facet_wrap(~ `Type of day`, ncol = 1)
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



