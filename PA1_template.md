---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---

Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
data_waNA <- data[which(data$steps!= "NA"), ]
```
What is mean total number of steps taken per day?

```r
library(plyr)
daily_steps <- ddply(data_waNA, .(date), summarise, steps=sum(steps))
hist(daily_steps$steps, xlab="steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
Mean number of steps per day is **1.0766189 &times; 10<sup>4</sup>**  
Median number of steps per day is **10765**

What is the average daily activity pattern?

```r
average_date <- ddply(data_waNA, .(interval), summarise, steps=mean(steps))
plot(average_date$interval, average_date$steps, type="l", xlab="5-minute interval", 
ylab="Average steps",main="Average daily activity")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_date[average_date$steps==max(average_date$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
colnames(average_date)[2] <- "intervalAvg"
```
## Imputing missing values

```r
#  missing values
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# Imputing NA's with average on 5-min interval
merged <- arrange(join(data, average_date), interval)
```

```
## Joining by: interval
```

```r
# the new dataset 
merged$steps[is.na(merged$steps)] <- merged$intervalAvg[is.na(merged$steps)]
# plot the histogram
new_daily_steps <- ddply(merged, .(date), summarise, steps=sum(steps))
hist(new_daily_steps$steps, main="Number of Steps", 
     xlab="steps taken each day",,)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(new_daily_steps$steps)
```

```
## [1] 10766.19
```

```r
median(new_daily_steps$steps)
```

```
## [1] 10766.19
```

```r
daily_steps_1 <- sum(data_waNA$steps)
daily_steps_2 <- sum(merged$steps)
diff <- daily_steps_2 -daily_steps_1 []
```
Mean values didn't change

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
weekdays <- weekdays(as.Date(merged$date))
data_weekdays <- transform(merged, day=weekdays)
data_weekdays$wk <- ifelse(data_weekdays$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
average_week <- ddply(data_weekdays, .(interval, wk), summarise, steps=mean(steps))

xyplot(steps ~ interval | wk, data = average_week, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
  
The graph shows that the patterns do not differ a lot between weekend and weekdays
