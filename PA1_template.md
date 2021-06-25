---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
file = "./repdata_data_activity.zip"
unzip(file,exdir=getwd())
data <- read.csv("activity.csv",na.strings ="NA",colClasses = c('integer','Date','factor')) 
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.


```r
table(data$date)
```

```
## 
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        288        288        288        288        288        288        288 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##        288        288        288        288        288        288        288 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##        288        288        288        288        288        288        288 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##        288        288        288        288        288        288        288 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##        288        288        288        288        288        288        288 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##        288        288        288        288        288        288        288 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##        288        288        288        288        288        288        288 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##        288        288        288        288        288
```

```r
sum_step <- tapply(data$steps, data$date, sum,na.rm=TRUE)
d1 <- data.frame(date = names(sum_step), sum = sum_step)
```

Histogram of the total number of steps taken each day


```r
library(ggplot2)
g <- ggplot(d1, aes(sum))
g + geom_histogram(color = "red")+
        labs(x= "Total number of steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](111_files/figure-html/hist1-1.png)<!-- -->

The mean and median of the total number of steps taken per day

```r
mean(sum_step, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(sum_step, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

A time series plot  of the 5-minute interval and the average number of steps taken, averaged across all days.


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.4     v dplyr   1.0.3
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
## v purrr   0.3.4
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
aver_activity <- tapply(data$steps, data$interval, mean, na.rm =T)
d2 <- data.frame(interval = as.numeric(names(aver_activity)), average = aver_activity)
g <- ggplot(d2, aes(interval,average))
g + geom_line(color = "blue", size = 0.8) + 
        labs(x= "5-minute interval") +
        labs(y = "Average number of steps taken")
```

![](111_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
d2[which.max(d2$average),]
```

```
##     interval  average
## 835      835 206.1698
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset


```r
table(is.na(data$steps)) ### The numbers of TURE represent the missing values
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

Create a new dataset that is equal to the original dataset but with the missing data filled in by using the mean for that 5-minute interval


```r
imputing <- data ### new dataset
for (i in 1:nrow(imputing)) {
        if(is.na(imputing[i,1])){
                imputing[i,1] <- d2[(d2$interval == imputing[i,3]),2]
        }
}
```

A histogram of the total number of steps taken each day

```r
table(imputing$date)
```

```
## 
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        288        288        288        288        288        288        288 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##        288        288        288        288        288        288        288 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##        288        288        288        288        288        288        288 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##        288        288        288        288        288        288        288 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##        288        288        288        288        288        288        288 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##        288        288        288        288        288        288        288 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##        288        288        288        288        288        288        288 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##        288        288        288        288        288
```

```r
sum_step3 <- tapply(imputing$steps, imputing$date, sum,na.rm=TRUE)
d3 <- data.frame(date = names(sum_step3), sum = sum_step3)
library(ggplot2)
g <- ggplot(d3, aes(sum))
g + geom_histogram(color = "white")+
        labs(x= "Total number of steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](111_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


The mean and median total number of steps taken per day with the missing data filled in.

```r
mean(sum_step3, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sum_step3, na.rm=TRUE)
```

```
## [1] 10766.19
```

After imputing missing data, the mean and median are higher. The main reason is that in the original data, for any `interval`, the value of `steps` for some days is `NA`. By default, the total number of steps taken these days is set to 0. However, after replacing the missing `steps` value with the average `steps` of the associated `interval` value, these 0 values will be removed from the histogram of the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

A new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
### Creating a  new dataset with factor variable with “weekday” and “weekend”
d4 <- data 
d4$days <- NA
for (i in 1:nrow(d4)) { 
        if(weekdays((d4$date)[i]) %in% c("Saturday","Sunday")){
                d4$days[i] = "weekend"
        } 
        else{
                d4$days[i] = "weekday"
        }

}
d4$days <- as.factor(d4$days)
table(d4$days)
```

```
## 
## weekday weekend 
##   12960    4608
```

A panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
## subset the date in weekdays
aver_weekday <- tapply(d4[d4$days == "weekday",]$steps, d4[d4$days == "weekday",]$interval, mean, na.rm =T)
aver_weekday_df <- data.frame(interval = as.numeric(names(aver_weekday)), average = aver_weekday) 
## subset the date in weekends
aver_weekend <- tapply(d4[d4$days == "weekend",]$steps, d4[d4$days == "weekend",]$interval, mean, na.rm =T)
aver_weekend_df <- data.frame(interval = as.numeric(names(aver_weekend)), average = aver_weekend)

all_average <- cbind(aver_weekday_df, aver_weekend_df$average)
colnames(all_average)[2] <- "weekday"
colnames(all_average)[3] <- "weekend"
mrg <- all_average %>% gather("days", "average", -interval) ## A tidy merged file
g <- ggplot(mrg, aes(interval,average))
g + geom_line(aes(color = days))  + facet_grid(rows = vars(days))+
        labs(x = "5-minute interval")+
        labs(y = "Average number of steps taken")
```

![](111_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

