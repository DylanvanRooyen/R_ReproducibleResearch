---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip")
}
activityData <- read.csv(file="activity.csv", header=TRUE)
```

## What is mean total number of steps taken per day?

```r
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/totsteps-1.png)<!-- -->

```r
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
```

## What is the average daily activity pattern?
# Time-series plot of 5 minute intervals and the average number
#  of steps taken averaged acoss all days.

```r
library(ggplot2)
meanStepsInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/time-series-1.png)<!-- -->


```r
maxInt <- meanStepsInt[which.max(meanStepsInt$steps),]
```
## Imputing missing values

```r
missingVals <- is.na(activityData$steps)
```
# Create a new dataset that is equal to the original dataset but with 
# the missing data filled in.

```r
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsInt$steps[match(activityData$interval,
                                             meanStepsInt$interval)],
                                             activityData$steps))
```
# Histogram of the total number of steps taken each day

```r
impStepsInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/histFilled-1.png)<!-- -->

```r
impMeanSteps <- mean(impStepsInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsInt$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsInt$steps) - sum(totalSteps$steps)
```
## Are there differences in activity patterns between weekdays and weekends?

```r
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)


meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/diffrence-1.png)<!-- -->
