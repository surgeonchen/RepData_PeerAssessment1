## Loading and preprocessing the data
file = "./repdata_data_activity.zip"
unzip(file,exdir=getwd())
data <- read.csv("activity.csv",na.strings ="NA",colClasses = c('integer','Date','factor')) 

## What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day.
table(data$date)
sum_step <- tapply(data$steps, data$date, sum,na.rm=TRUE)
d1 <- data.frame(date = names(sum_step), sum = sum_step)

## Histogram of the total number of steps taken each day
png("plot1.png",480,480)
library(ggplot2)
g <- ggplot(d1, aes(sum))
g + geom_histogram(color = "red")+
                labs(x= "Total number of steps taken each day")
dev.off()
## The mean and median of the total number of steps taken per day
mean(sum_step, na.rm=TRUE)
median(sum_step, na.rm=TRUE)

## What is the average daily activity pattern?
## A time series plot  of the 5-minute interval and the average number of steps taken, averaged across all days.
library(tidyverse)
aver_activity <- tapply(data$steps, data$interval, mean, na.rm =T)
d2 <- data.frame(interval = as.numeric(names(aver_activity)), average = aver_activity)
png("plot2.png",480,480)
g <- ggplot(d2, aes(interval,average))
g + geom_line(color = "blue", size = 1) + 
        labs(x= "5-minute interval") +
        labs(y = "Average number of steps taken")
dev.off() 
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
d2[which.max(d2$average),]




## Imputing missing values

## Calculate and report the total number of missing values in the dataset

table(is.na(data$steps))  ### The numbers of TURE represent the missing values

## Create a new dataset that is equal to the original dataset but with the missing data filled in by using the mean for that 5-minute interval
imputing <- data
for (i in 1:nrow(imputing)) {
        if(is.na(imputing[i,1])){
                imputing[i,1] <- d2[(d2$interval == imputing[i,3]),2]
        }
}

## A histogram of the total number of steps taken each day 
table(imputing$date)
sum_step3 <- tapply(imputing$steps, imputing$date, sum,na.rm=TRUE)
d3 <- data.frame(date = names(sum_step3), sum = sum_step3)
png("plot3.png",480,480)
library(ggplot2)
g <- ggplot(d3, aes(sum))
g + geom_histogram(color = "white")+
        labs(x= "Total number of steps taken each day")
dev.off()

## The mean and median total number of steps taken per day with the missing data filled in.
mean(sum_step3, na.rm=TRUE)
median(sum_step3, na.rm=TRUE)


## After imputing missing data, the mean and median are higher. The main reason is that in the original data, for any `interval`, the value of `steps` for some days is `NA`. By default, the total number of steps taken these days is set to 0. However, after replacing the missing `steps` value with the average `steps` of the associated `interval` value, these 0 values will be removed from the histogram of the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

## A new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


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


## A panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
aver_weekday <- tapply(d4[d4$days == "weekday",]$steps, d4[d4$days == "weekday",]$interval, mean, na.rm =T)
aver_weekday_df <- data.frame(interval = as.numeric(names(aver_weekday)), average = aver_weekday)
aver_weekend <- tapply(d4[d4$days == "weekend",]$steps, d4[d4$days == "weekend",]$interval, mean, na.rm =T)
aver_weekend_df <- data.frame(interval = as.numeric(names(aver_weekend)), average = aver_weekend)

all_average <- cbind(aver_weekday_df, aver_weekend_df$average)
colnames(all_average)[2] <- "weekday"
colnames(all_average)[3] <- "weekend"
mrg <- all_average %>% gather("days", "average", -interval)
png("plot4.png",480,480)
        g <- ggplot(mrg, aes(interval,average))
        g + geom_line(aes(color = days))  + facet_grid(rows = vars(days))+
                labs(x = "5-minute interval")+
                labs(y = "Average number of steps taken")
dev.off()