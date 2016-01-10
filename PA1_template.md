# Reproducible Research: Peer Assessment 1
library(lubridate)

library(dplyr)

library(ggplot2)

## Loading and preprocessing the data
activity <- read.csv("./data/activity.csv", stringsAsFactors = FALSE)


## What is mean total number of steps taken per day?

#### 1. Transform date from character to date format and get total steps per day
summaryActivity <- activity %>% transform(date = ymd(date)) %>% group_by(date) %>% summarise(steps = sum(steps, na.rm = TRUE))

#### 2. Make a histogram of the total number of steps taken each day 

hist(summaryActivity$steps, col = "green", main = "Histogram of total daily steps",
     xlab = "Steps", breaks = 10)
     
abline(v = mean(summaryActivity$steps), lwd = 2, lty = 1, col = "purple")

abline(v = median(summaryActivity$steps), lwd = 2, lty = 1, col = "blue")

legend(x= 13000, y= 16, lwd = 2, lty = 1, col = c("purple","blue"), legend = c("Mean", "Median"), bty="n", y.intersp = .5)

#### 3. Report the mean and median of the total number of steps taken per day

##### * Mean
mean(summaryActivity$steps)

##### * Median
median(summaryActivity$steps)

## What is the average daily activity pattern?

#### 1. Transform date from character to date format and get average steps
summaryInterval <- activity %>% transform(date = ymd(date)) %>% 
    group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE))
    
    
#### 2. Make a time series plot
with(summaryInterval, plot(interval, steps, type = "l", col = "blue", lwd = 2,
                           main = "Average Number of Steps Taken per Interval",
                           xlab = "Interval in minutes",
                           ylab = "Average Number of Steps"))
                           
                           
#### 3. Which 5-minute interval contains the maximum number of steps?

filter(summaryInterval, steps == max(steps))


## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with  NA s)

table(complete.cases(activity))

##### Number of missing values are the FALSE count

#### 2. Devise a strategy for filling in all of the missing values in the dataset.
meanStep <- mean(activity$steps,na.rm = TRUE)

##### Use the mean value for the missing items 

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityNew <- activity
activityNew$steps[is.na(activityNew$steps)] <- meanStep

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
summaryActivityNew <- activityNew %>% transform(date = ymd(date)) %>% group_by(date) %>% summarise(steps = sum(steps, na.rm = TRUE))

hist(summaryActivityNew$steps, col = "green", main = "Histogram of total daily steps",
     xlab = "Steps", breaks = 10)
     
abline(v = mean(summaryActivityNew$steps), lwd = 2, lty = 1, col = "purple")

abline(v = median(summaryActivityNew$steps), lwd = 2, lty = 1, col = "blue")

legend(x= 13000, y= 16, lwd = 2, lty = 1, col = c("purple","blue"), legend = c("Mean", "Median"), bty="n", y.intersp = .5)

##### * Mean
mean(summaryActivityNew$steps)

##### * Median
median(summaryActivityNew$steps)

##### There is an increase in Mean and Median

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend"


activity$day <- ifelse(as.POSIXlt(activity$date)$wday %in% c(0,6), "weekend", "weekday")

activity <- transform(activity, day = factor(day, labels = list("weekday", "weekend")))

#### 2. Make a panel plot containing a time series plot 

qplot(interval, steps, data = SumActivity, geom = "line", facets = day~.)
