---
title: "Reproducible Research: Project 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
* Dataset : [Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 


## Loading and preprocessing the data
unzip the data from url and load the data in to data.table


```r
library("data.table")
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "activitydata.zip")
unzip("activitydata.zip",exdir = "data")


activity <- fread(input ="data/activity.csv")
```

## What is mean total number of steps taken per day?

   Calculate the total number of steps taken per day


```r
TotalSteps <- activity[, lapply(.SD, sum),.SDcols="steps",by=date]
head(TotalSteps,15)
```

```
##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
## 11: 2012-10-11 10304
## 12: 2012-10-12 17382
## 13: 2012-10-13 12426
## 14: 2012-10-14 15098
## 15: 2012-10-15 10139
```

   Make a histogram of the total number of steps taken each day
   

```r
library(ggplot2)
ggplot(TotalSteps,aes(x=steps))+
  geom_histogram(binwidth=2000, fill="#282649")+
  labs(title ="Total  Number of steps per day", x="Steps", y="Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/histogram of steps-1.png)<!-- -->

   Calculate and report the mean and median of the total number of steps taken per day
   

```r
TotalSteps[, .(Mean_total_steps = mean(steps,na.rm=TRUE),
               Median_total_steps = median(steps,na.rm=TRUE))]
```

```
##    Mean_total_steps Median_total_steps
## 1:         10766.19              10765
```

## What is the average daily activity pattern?

   Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
   

```r
IntervalActivity <- activity[, lapply(.SD,mean,na.rm=TRUE),.SDcols="steps", by=interval]

ggplot(IntervalActivity,aes(x=interval,y=steps))+
  geom_line(color="blue",size=1)+
  labs(title="Avergae Steps taken daily",x="Interval",y="Average Steps")
```

![](PA1_template_files/figure-html/interval plot-1.png)<!-- -->

   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
   

```r
IntervalActivity[steps==max(steps), .(interval)]
```

```
##    interval
## 1:      835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  

```r
activity[is.na(steps), .N]
```

```
## [1] 2304
```

  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  

```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
activitynew <- copy(activity)
activitynew[, steps := as.double(steps)]
activitynew[, steps := lapply(.SD, na.aggregate, median), by = interval,.SDcols = "steps"]
```

  Create a new dataset that is equal to the original dataset but with the missing data filled in.
  

```r
fwrite(activitynew,file="data/activitynew.csv")
```

   Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
   

```r
TotalStepsnew <- activitynew[, lapply(.SD, sum),.SDcols="steps",by=date]
TotalStepsnew[, .(Mean_total_steps = mean(steps,na.rm=TRUE),
               Median_total_steps = median(steps,na.rm=TRUE))]
```

```
##    Mean_total_steps Median_total_steps
## 1:         10766.19           10766.19
```

```r
ggplot(TotalStepsnew,aes(x=steps))+
  geom_histogram(binwidth=2000, fill="#282649")+
  labs(title ="Total  Number of steps per day", x="Steps", y="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Type|Mean_total_steps|Median_total_steps

First part|10766.19|10765

Second Part|10766.19|10766.19



## Are there differences in activity patterns between weekdays and weekends?
