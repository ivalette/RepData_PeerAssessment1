# Reproducible Research: Peer Assessment 1
This is the report produced for the Peer Assignment 1 of "Reproducible Research" in Coursera.


## Loading and preprocessing the data

We load the data and look at the first 5 rows.

```r
setwd("/Users/isa/Documents/Courses/Johns Hopkins/Reproducible Research/Week2/RepData_PeerAssessment1")
data <- read.csv("activity.csv", colClasses = c("numeric", "factor", "numeric"))
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

We transform the data to adequate format.

```r
library(lubridate)
data$date2=as.Date(strptime(data$date, format="%Y-%m-%d" ),"%Y%m%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  $ date2   : Date, format: "2012-10-01" "2012-10-01" ...
```

```r
summary(data)
```

```
##      steps                date          interval          date2           
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0   Max.   :2012-11-30  
##  NA's   :2304     (Other)   :15840
```


## What is mean total number of steps taken per day?
First, we aggregate the total number of steps taken per day and create a histogram that shows it.

```r
agg <- with(data, aggregate(data$steps, by=list(data$date2), sum))
barplot(agg$x, names.arg = agg$Group.1, col="red", main="Total number of steps taken per day", xlab="Days: From October 1st to December 1st",ylab="Steps Frequency")
```

![](PA1_template_files/figure-html/scatterplot-1.png) 
We then calculate the mean and the median as shown respectively:

```r
mean(agg[, "x"], na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(agg[, "x"], na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
First, we aggregate the number of steps per 5 min. intervals and plot the average daily activity pattern.


```r
agg2 <- with(data, aggregate(data$steps, by=list(data$interval), mean, na.rm = TRUE))
plot(agg2$Group.1, agg2$x, type = "l", xlab = "5 Minutes Intervals", ylab = "Steps Frequency", 
     main="Average daily activity pattern")
```

![](PA1_template_files/figure-html/scatterplot2-1.png) 



The interval with the highest averaged number of steps is as follows: 835. The highest averaged number of steps is 206.17, as shown in the graf above.

## Imputing missing values
The total number of "NA" (missing values) is:

```r
sum(is.na(data [,1]))
```

```
## [1] 2304
```
We replace missing values with the "5 minutes interval" mean and create a new dataset.

```r
names(agg2)[names(agg2) == 'Group.1'] <- 'interval'
names(agg2)[names(agg2) == 'x'] <- 'newsteps'
data2 <- merge(data, agg2, by = "interval")
data2$steps[is.na(data2$steps)] <- data2$newsteps[is.na(data2$steps)]
##Create a new dataset
newdata <- subset(data2, select = c("steps","interval","date2"))
```
Then, we re-aggregate the total number of steps taken per day and create a histogram that shows it.

```r
agg3 <- with(newdata, aggregate(newdata$steps, by=list(newdata$date2), sum))
barplot(agg3$x, names.arg = agg3$Group.1, col="red", main="Total number of steps taken per day (with imputed value)", xlab="Days: From October 1st to December 1st",ylab="Step Frequency")
```

![](PA1_template_files/figure-html/scatterplot3-1.png) 
We calculate the mean and the median, as shown respectively:

```r
mean(agg3[, "x"], na.rm = F)
```

```
## [1] 10766.19
```

```r
median(agg3[, "x"], na.rm = F)
```

```
## [1] 10766.19
```
Conclusion: As expected, the effect of replacing missing values with the "5 min. interval" mean has no impact on the mean but has a very low effect on the median, equaling it exactly to the mean.

## Are there differences in activity patterns between weekdays and weekends?
We create a new variable showing the type of week days.

```r
## Create a new variable (factor) for type of days
newdata$date <- weekdays(newdata$date2)

newdata$daytype <- c("Weekday")
for (i in 1:nrow(newdata)){
  if (newdata$date[i] == "Saturday" || newdata$date[i] == "Sunday")
          {
    newdata$daytype[i] <- "Weekend"
          }
}
newdata$daytype <- as.factor(newdata$daytype)

# Aggregate the total number of steps taken per day.
agg4 <- with(newdata, aggregate(newdata$steps, by=list(newdata$interval, newdata$daytype), mean))
```

# Make the panel plot for weekdays and weekends
Finally, we plot the average daily activity pattern for weekdays and weekends.

```r
library(ggplot2)
qplot(Group.1, x, data=agg4, geom=c("line"), xlab="5 Minutes Intervals", 
      ylab="Averaged Number of steps", main="Average daily activity pattern for Weekday and Weekend (with imputed values)") + facet_wrap(~ Group.2, ncol=1)
```

![](PA1_template_files/figure-html/qplot-1.png) 
Conclusion: The averaged daily activity patterns seem to be more active (with a highter number of steps) for the weekends than the weekdays on average as confirmed by the means for weekday and weekend below:

```r
tapply(agg4$x, agg4$Group.2, mean, na.rm = F)
```

```
##  Weekday  Weekend 
## 35.61058 42.36640
```
