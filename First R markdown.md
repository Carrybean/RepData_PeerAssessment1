---
title: "Reproducible Research Peer Graded Assignment"
author: "CarryBean"
date: "11/23/2020"
output: html_document
---



## Loading and preprocessing the data

First, we load the data

```r
data <- read.csv("activity.csv", header = TRUE)
```
then, we process it into a fromat suitable for our analysis.

## Data Visualization

In this part, we'll try to make a histogram of the total number of steps taken each day.


```r
library(dplyr)
library(tidyr)

Total_steps <-
  summarize(
    group_by(tbl_df(data),date)
        ,sum = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot <- hist(Total_steps$sum,  main = paste("Histogram of" , "the total number of steps taken each day"), xlab = "Total number of steps taken each day")
```

![plot of chunk visualize , echo TRUE](figure/visualize , echo TRUE-1.png)

```r
Mean_Total_Steps <- mean(Total_steps$sum,na.rm = TRUE)
Median_Total_Steps <- median(Total_steps$sum,na.rm = TRUE)
```

The mean and the median of the total number of steps taken each day, respectively,

```r
Mean_Total_Steps
```

```
## [1] 10766.19
```
and

```r
Median_Total_Steps
```

```
## [1] 10765
```

## The average daily activity pattern

Below is a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 

```r
Mean_steps <-
  summarize(
    group_by(tbl_df(data),interval)
    ,mean = mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(Mean_steps$interval, Mean_steps$mean, type = "l", xlab = "5-min interval", ylab = "Average across all Days", main = "Average number of steps taken", col = "blue")
```

![plot of chunk the time serie](figure/the time serie-1.png)

```r
max_interval <- Mean_steps$interval[which.max(Mean_steps$mean)]
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

The answer is

```r
max_interval
```

```
## [1] 835
```

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

The total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
NAs_steps <- sum (is.na(data$steps))
```

The total number of rows with NAs is

```r
NAs_steps
```

```
## [1] 2304
```

The strategy for filling in all of the missing values in the dataset. 


```r
Steps_Average <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(Steps_Average, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
```


The new dataset that is equal to the original dataset but with the missing data filled in.


```r
New_data <- data
New_data$steps <- fillNA
```


A histogram of the total number of steps taken each day 


```r
Total_steps_1 <-
  summarize(
    group_by(tbl_df(New_data),date)
        ,sum = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot <- hist(Total_steps_1$sum,  main = paste("Histogram of" , "the total number of steps taken each day "), xlab = "Total number of steps taken each day")
```

![plot of chunk new hist](figure/new hist-1.png)

```r
Mean_Total_Steps_1 <- mean(Total_steps_1$sum,na.rm = TRUE)
Median_Total_Steps_1 <- median(Total_steps_1$sum,na.rm = TRUE)
```

The mean and median total number of steps taken per day are, respectively, 

```r
Mean_Total_Steps_1
```

```
## [1] 10766.19
```
and

```r
Median_Total_Steps_1
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? 


```r
Mean_Total_Steps <- mean(Total_steps$sum)
Mean_Total_Steps
```

```
## [1] NA
```


```r
Median_Total_Steps <- median(Total_steps$sum)
Median_Total_Steps
```

```
## [1] NA
```

After replacing the mean is the same but the median is a little bit different

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
require("lubridate")

New_data$date<-as.Date(New_data$date, "%Y-%m-%d")
New_data_1 <- cbind(New_data,weekdays(New_data$date))
colnames(New_data_1)<-c(colnames(New_data),"Weekday")
New_data_2 <- numeric()
for (i in 1:nrow(New_data_1)){
  row <-New_data_1[i,]
  if (row$Weekday == "Saturday" || row$Weekday == "Sunday"){
    row$Weekday <- "Weekend"
  } else{
    row$Weekday <- "Weekday"
  }
  New_data_2 <- rbind(New_data_2,row)
}
```


A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
require("lattice")

New_data_2$Weekday <- factor(New_data_2$Weekday)

stepsByDay <- aggregate(steps ~ interval + Weekday, data = New_data_2, mean)
names(stepsByDay) <- c("interval", "Weekday", "steps")

xyplot(steps ~ interval | Weekday, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk final plot](figure/final plot-1.png)











