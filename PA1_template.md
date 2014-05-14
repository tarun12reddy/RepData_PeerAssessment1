Reproducible Research - Assignment 1
========================================================

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
```


## Removing the missing data

```r
x <- data$steps[!is.na(data$steps)]
y <- data$date[!is.na(data$steps)]
```


- Make a histogram of the total number of steps taken each day


```r
sum_summary <- tapply(x, y, sum)
hist(sum_summary, xlab = "Steps", main = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


- Calculate and report the mean and median total number of steps taken per day

```r
mean_summary <- tapply(x, y, mean)
mean_summary[is.na(mean_summary)] <- 0
median_summary <- tapply(x, y, median)
median_summary[is.na(median_summary)] <- 0
summary(mean_summary)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0    23.5    36.1    32.5    44.5    73.6
```

```r
summary(median_summary)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0       0       0       0
```


## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
z <- data$interval[!is.na(data$steps)]

mean1_summary <- tapply(x, as.factor(z), mean)
plot(as.numeric(attributes(factor(data$interval))$levels), mean1_summary, type = "l", 
    xlab = "Interval", ylab = "Number of steps", xaxt = "n", xlim = c(0, 2355))
axis(1, at = c(0, 500, 1000, 1500, 2000))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
table1 <- cbind(mean1_summary, as.numeric(attributes(factor(data$interval))$levels))
table1 <- table1[order(table1[, 1]), ]
table1[288, 2]
```

```
## [1] 835
```


## Inputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(data[, 1]) - length(x)
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc

```r
"Use the daily average mean in place of missing value"
```

```
## [1] "Use the daily average mean in place of missing value"
```


- Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
options(warn = -1)
data$steps[is.na(data$steps)] <- "NA"
k <- 1
for (i in 1:length(data[, 1])) {
    if (as.character(data$steps[i]) == "NA") {
        if (i <= k * summary(data$date)[1]) {
            data$steps[i] <- mean_summary[k]
            data$steps[i] <- as.numeric(data$steps)[i]
        }
    }
    if (i == k * summary(data$date)[1]) {
        k <- k + 1
    }
}
data$steps <- as.numeric(data$steps)
```

- Make a histogram of the total number of steps taken each day 


```r
new_sum_summary <- tapply(data$steps, data$date, sum)
hist(new_sum_summary, xlab = "Steps", main = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


- Calculate and report the mean and median total number of steps taken per day.

```r
new_mean_summary <- tapply(data$steps, data$date, mean)
new_median_summary <- tapply(data$steps, data$date, median)
summary(new_mean_summary)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0    23.5    36.1    32.5    44.5    73.6
```

```r
summary(new_median_summary)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0       0       0       0
```


- Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
boxplot(mean_summary, new_mean_summary, median_summary, new_median_summary, 
    xaxt = "n", ylab = "Number of Steps", main = "Box Plots")
axis(1, at = c(1, 2, 3, 4), labels = c("OriginalMean", "ModifiedMean", "OriginalMedian", 
    "ModifiedMedian"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
"No Difference in this method of replacement of missing value"
```

```
## [1] "No Difference in this method of replacement of missing value"
```


## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

```r
data$date <- as.Date(data$date)
days <- weekdays(data$date)
for (i in 1:length(days)) {
    if (days[i] == "Monday" || days[i] == "Tuesday" || days[i] == "Wednesday" || 
        days[i] == "Thursday" || days[i] == "Friday") {
        days[i] <- "weekday"
    }
    if (days[i] == "Saturday" || days[i] == "Sunday") {
        days[i] <- "weekend"
    }
}
days <- factor(days)

data[, 4] <- days
summary(days)
```

```
## weekday weekend 
##   12960    4608
```


- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
s1 <- split(data$steps, data$V4)
s2 <- split(data$interval, data$V4)
weekday_table <- cbind(s1$weekday, s2$weekday)
weekday_table[, 2] <- factor(weekday_table[, 2])
weekday_mean <- tapply(weekday_table[, 1], weekday_table[, 2], mean)

weekend_table <- cbind(s1$weekend, s2$weekend)
weekend_table[, 2] <- factor(weekend_table[, 2])
weekend_mean <- tapply(weekend_table[, 1], weekend_table[, 2], mean)

plot(as.numeric(attributes(factor(data$interval))$levels), weekday_mean, type = "l", 
    xlab = "Interval", ylab = "Number of steps", xaxt = "n", xlim = c(0, 2355), 
    main = "Weekday")
axis(1, at = c(0, 500, 1000, 1500, 2000))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-141.png) 

```r
plot(as.numeric(attributes(factor(data$interval))$levels), weekend_mean, type = "l", 
    xlab = "Interval", ylab = "Number of steps", xaxt = "n", xlim = c(0, 2355), 
    main = "Weekend")
axis(1, at = c(0, 500, 1000, 1500, 2000))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-142.png) 

