# Loading and preprocessing the data
data_na<- read.csv("activity.csv", na.strings = "NA", header = TRUE)
data<- data_na[!is.na(data_na),]

# sum all the steps in the dataset with respect to date
agg<- aggregate(steps ~ date, data, sum)

## Histogram indicates the number of observations which lie in-between the range of values, known as class or bin.

# Histogram of the total number of steps taken each day

png(filename = "histogram.png")

hist(agg$steps, xlab = "steps", ylim = c(0,30), main = "Histogram of the total number of steps taken each day")

dev.off()

# Mean and median number of steps taken each day

mean_steps<-  mean(agg$steps)
median_steps<- median(agg$steps)

# Time series plot of the average number of steps taken

data_averaged<- aggregate(steps ~ interval, data, mean)

png(filename = "timeSeries.png")

plot(data_averaged$interval, data_averaged$steps, type= "l", xlab= "5-min interval", ylab= "Average number of steps", main= "average number of steps taken")

dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maximum_steps<- data_averaged[data_averaged$steps == max(data_averaged$steps),][1]

# Imputing missing values

no_of_missing_values<- nrow(data_na[is.na(data_na),])

data_mean<- aggregate(steps~date, data, mean)

data_new<- data_na
for(i in 1:nrow(data_new)) {
        
        if(is.na(data_new$steps[i])){
                
                index<- which(data_averaged$interval==data_na$interval[i])
                data_new$steps[i]<- data_averaged[index,][2]
                
                
                
        }
        
}

data_new$steps<- as.numeric(data_new$steps)

# Make a histogram of the total number of steps taken each day for new data

png(filename = "histogram_newData.png")

agg_new<- aggregate(steps ~ date, data_new, sum)
hist(as.numeric(agg_new$steps), xlab = "steps", main = "Histogram of the total number of steps taken each day")

dev.off()

# Calculate and report the mean and median total number of steps taken per day

mean_steps_new<-  mean(agg_new$steps)
median_steps_new<- median(agg_new$steps)

# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

data_new['Type of day']<- weekdays(as.POSIXct.Date(data_new$date))
data_new$`Type of day`[data_new$`Type of day` %in% c("Saturday", "Sunday")]<- "weekend"
data_new$`Type of day`[data_new$`Type of day` != "weekend"]<- "weekday"

# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

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





