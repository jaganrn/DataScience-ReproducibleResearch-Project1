# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
# load the activity data by unzipping the activity.zip
setwd("c:/usr/jagan/DataScience/ReproducibleResearch/Week1/DataScience-ReproducibleResearch-Project1")
if(!file.exists("activity.csv")) { #unzip for the first time
    unzip("activity.zip")
}
activityData <- read.csv("activity.csv")
head(activityData)
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

## What is mean total number of steps taken per day?
In this section missing values are ignored during the computations


```r
# compute mean total number of steps taken per day
# draw histogram of total number of steps per day
stepsByDay <- aggregate(steps ~ date, data=activityData, FUN=sum, na.rm=TRUE)
hist(stepsByDay$steps, main="Histogram of Number of Steps per Day", xlab="Number of Steps/Day", col=blues9)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
# compute mean & median steps per day
# summary(stepsByDay$steps) can also be used with options(digits=10)
mean(stepsByDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
averageStepsBy5MinInterval <- aggregate(steps ~ interval, data=activityData, FUN=mean, na.rm=TRUE)
plot(averageStepsBy5MinInterval$interval, averageStepsBy5MinInterval$steps, type="l", xlab="interval", ylab="Average Steps", col="purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
averageStepsBy5MinInterval[which.max(averageStepsBy5MinInterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

As there are a number of days/intervals where there are missing values (coded as NA), in this step the missing values are filled by the average value for the slot across all the days


```r
# fill the NA values with the average steps for that interval across all days
totalRecords <- length(activityData$steps)
for(i in 1:totalRecords) {
    if(is.na(activityData[i,1])) {
        index <- i%%288;
        if(index==0) {
            index <- 288  # special case as R indexes from 1..N
        }
        activityData[i,1] <- averageStepsBy5MinInterval[index, 2]
    }  
}

stepsByDayFilledData <- aggregate(steps ~ date, data=activityData, FUN=sum)
hist(stepsByDayFilledData$steps, main="Histogram of Number of Steps per Day", xlab="Number of Steps/Day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
# compute mean & median steps per day
# summary(stepsByDayFilledData$steps) can also be used with options(digits=10)
mean(stepsByDayFilledData$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDayFilledData$steps)
```

```
## [1] 10766.19
```

By looking at the filled data I do not see much difference in the aggregate data. The only difference is a slight change in the median value

## Are there differences in activity patterns between weekdays and weekends?



```r
library(rmarkdown)
library(lattice)
# added a new column named dateCategory to indicate Weekday (Mon-Fri) or Weekend (Sat-Sun). POSIXlt$wday range is 0..6 with 0 as Sun
activityData$dateCategory <- ifelse(as.POSIXlt(activityData$date)$wday %in% c(1,2,3,4,5), "Weekday", "Weekend")
averageStepsBy5MinInterval <- aggregate(steps ~ interval+dateCategory, data=activityData, FUN=mean)

# plot separate graphs for Weekend and Weekday
xyplot(steps ~ interval | dateCategory, averageStepsBy5MinInterval, type="l", layout=c(1,2), xlab="Interval", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
averageStepsBy5MinInterval[which.max(averageStepsBy5MinInterval$steps),]$interval
```

```
## [1] 835
```

NOTE: generate the html & md files using rmarkdown::render function as I had difficulty in running *knitr2html()* function. Based on the plots I see minor difference in the patten of activities on Weekend compared to Weekday






