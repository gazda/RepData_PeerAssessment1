# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The data is loaded into `data` and then all missing values were removed getting thus the `clean_data`.


```r
library(plyr)
library(lattice)

unzip('activity.zip')
data <- read.csv('activity.csv')
data$date <- as.POSIXct(data$date)
clean_data <- data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?


```r
daily_steps <- ddply(clean_data, ~date, summarise, total_steps = sum(steps))
hist(daily_steps$total_steps, breaks = 'FD',
     xlab = 'Total number of daily steps',
     main = 'Histogram of Total number of daily steps')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and median are:

```r
mean(daily_steps$total_steps)
```

```
## [1] 10766
```

```r
median(daily_steps$total_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?



```r
interval_steps <- ddply(clean_data, ~interval, summarise, average_steps = mean(steps))

plot(interval_steps$interval, interval_steps$average_steps,
     type = 'l', xlab = 'Interval', ylab = 'Average steps',
     main = 'Average steps per interval')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Maximal average number of steps

```r
max_average_steps <- max(interval_steps$average_steps)
max_average_steps
```

```
## [1] 206.2
```

The interval with most steps is:

```r
interval_steps[interval_steps$average_steps == max_average_steps,]$interval
```

```
## [1] 835
```


## Imputing missing values

Total number of missing values:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Missing values were filled with the mean for that 5-minute interval.

```r
imputed_data <- ddply(data, .(interval), function(df) {
  df$steps[is.na(df$steps)] <- mean(df$steps, na.rm=TRUE)
  return(df)
  })
```


```r
imputed_daily_steps <- ddply(imputed_data, ~date, summarise, total_steps = sum(steps))
hist(imputed_daily_steps$total_steps, breaks = 'FD',
     xlab = 'Total number of daily steps',
     main = 'Histogram of Total number of daily steps')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

The imputed mean and median are:

```r
mean(imputed_daily_steps$total_steps)
```

```
## [1] 10766
```

```r
median(imputed_daily_steps$total_steps)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?


```r
imputed_data$date_type <- as.factor(sapply(imputed_data$date, function(d) {
  week_day <- weekdays(d)
  if(week_day == 'Saturday' || week_day == 'Sunday')
    'weekend'
  else
    'weekday'
  }))

imputed_interval_steps <- ddply(imputed_data, ~date_type+interval,
                                summarise, average_steps = mean(steps))

xyplot(average_steps~interval|date_type, imputed_interval_steps,
       type='l', layout=c(1,2),
       xlab = 'Interval', ylab = 'Number of steps')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
