# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data
Firstly, we'll download and read the csv file to a variable "activity":

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "data.zip")
unzip("data.zip")
activity <- read.csv("activity.csv",colClasses = c("numeric","Date","numeric"), stringsAsFactors = FALSE)
```

Additionally, we'll load some useful libraries for subsequent use. 

```r
library(dplyr)
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
library(lattice)
```

### What is mean total number of steps taken per day?
####1. Make a histogram  of the total number of steps taken each day

```r
all_daily_steps <- activity %>% group_by(date) %>% summarise(steps=sum(steps, na.rm = TRUE))
hist(all_daily_steps$steps, breaks = 20, col = "pink", xlab = "Daily steps", main = "Daily steps histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



####2. Calculate and report the mean and median total number of steps taken per day
First we'll calculate the mean and median

```r
mean_steps <- mean(all_daily_steps$steps, na.rm = TRUE)
median_steps <- median(all_daily_steps$steps, na.rm = TRUE)
```

```r
print(paste("The mean of the total number of steps taken per day equals",round(mean_steps,2)))
```

```
## [1] "The mean of the total number of steps taken per day equals 9354.23"
```

```r
print(paste("The median of the total number of steps taken per day equals",round(median_steps,2)))
```

```
## [1] "The median of the total number of steps taken per day equals 10395"
```

### What is the average daily activity pattern?
####1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days (y-axis)
We'll create an additional data frame, this time grouped by the interval.

```r
interval_steps <- activity %>% group_by(interval) %>% summarise(average_steps=mean(steps, na.rm = TRUE))
```
We'll now plot the average daily activity over the 5-minute intervals

```r
plot(x = interval_steps$interval, y = interval_steps$average_steps, type = "l", col = "orange", xlab = "5 minute interval", ylab = "Average number of steps across all days", main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_of_max <- interval_steps$interval[grepl(max(interval_steps$average_steps),interval_steps$average_steps)]
print(paste("The 5-minute interval that contains the maximum number of steps is:",interval_of_max,". This matches the above chart."))
```

```
## [1] "The 5-minute interval that contains the maximum number of steps is: 835 . This matches the above chart."
```

### Imputing missing values
####1. Calculate and report the total number of missing values in the dataset

```r
print(paste("The number of missing values is:", length(activity$steps[is.na(activity$steps)==TRUE])))
```

```
## [1] "The number of missing values is: 2304"
```
####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
In this example, to replace any missing values, I will use the average steps that match the same interval as the missing value. For instance, in a row where the interval is 5 and the steps value is missing - I will use the average steps for interval = 5 across the data.

####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filled_steps <- merge(x=activity, y=interval_steps,by="interval",all.x=TRUE)
filled_steps$imputed <- ifelse(is.na(filled_steps$steps),filled_steps$average_steps,filled_steps$steps)
```
####4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
imputed_daily_steps <- filled_steps %>% group_by(date) %>% summarise(imputed_steps=sum(imputed))
hist(imputed_daily_steps$imputed_steps, breaks = 20, col = "light green", xlab = "Daily steps", main = "Imputed daily steps histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean_imputed <- mean(imputed_daily_steps$imputed_steps)
median_imputed <- median(imputed_daily_steps$imputed_steps)
print(paste("The mean of the total number of steps, including imputed values, taken per day equals",round(mean_imputed,2)))
```

```
## [1] "The mean of the total number of steps, including imputed values, taken per day equals 10766.19"
```

```r
print(paste("The median of the total number of steps, including imputed values, taken per day equals",round(median_imputed,2)))
```

```
## [1] "The median of the total number of steps, including imputed values, taken per day equals 10766.19"
```


####Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Both the histogram and the reported center metrics show a significant difference from the estimates from the first part of the assignment. The main impact is centralization and standardization - by filling in the missing values with the relevant averages - we essentially centered the data around the mean and median, and brought them much closer together.

### Are there differences in activity patterns between weekdays and weekends?
####1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
filled_steps$weekday <- weekdays(filled_steps$date)
filled_steps$weekendFactor <- ifelse(filled_steps$weekday == "Sunday" | filled_steps$weekday == "Saturday","Weekend","Weekday")
```

####2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
This exercise if very similar to the one in the second part of the assignment, only now we need to break the plot into two panels, one for weekdays, and one for weekends.

```r
weekend_interval_steps <- filled_steps %>% group_by(weekendFactor, interval) %>% summarise(average_steps=mean(imputed))
xyplot(weekend_interval_steps$average_steps~weekend_interval_steps$interval | weekend_interval_steps$weekendFactor, layout = c(1,2), type = "l", xlab = "Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
