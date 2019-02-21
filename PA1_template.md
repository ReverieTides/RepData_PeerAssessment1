---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

 Course Project 1
===================

Data load

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
data <- read.csv("C:\\Users\\Dan\\Desktop\\CourseraDataScience\\Mod 5\\Week 1\\activity.csv")
data$date <- ymd(data$date)
```

# Question 1:
What is mean total number of steps taken per day?

1) Calculate the total number of steps taken per day


```r
hdata <- data %>%
    group_by(date) %>%
    summarise(totalStepsTaken = sum(steps,na.rm = TRUE))
```

2) If you do not understand the difference between a histogram and a barplot, 
research the difference between them. Make a histogram of the total number of
steps taken each day


```r
hist(hdata$totalStepsTaken,col = "red",main = "Histogram of total steps taken per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/q1.2-1.png)<!-- -->

3) Calculate and report the mean and median of the total number of steps taken per day


```r
mean(hdata$totalStepsTaken, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(hdata$totalStepsTaken, na.rm = TRUE)
```

```
## [1] 10395
```
# Question 2:
What is the average daily activity pattern?

1) Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
tdata <- data %>%
    group_by(interval) %>%
    summarise(meanSteps = mean(steps,na.rm = TRUE))

with(tdata,plot(interval, meanSteps, type = "l", lwd = 3, col = "red",xlab = "Time interval", ylab = "Mean Steps", main = "Time Interval Plot"))
```

![](PA1_template_files/figure-html/q2.1-1.png)<!-- -->

2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxint <- tdata[which.max(tdata$meanSteps),1]
maxint
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

# Question 3:
Imputing missing values

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

 - I will use the mean for that interval


```r
dayMeans <- data %>%
    group_by(interval) %>%
    summarise(averageSteps = mean(steps, na.rm = TRUE))
```

3) Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
completeData <- data

patchNAs <- function(completeData, dayMeans){
    for(i in 1:nrow(completeData)){
    
        if(is.na(completeData[i,"steps"])){
            completeData[i,"steps"] <- dayMeans[match(x = completeData[i,"interval"], table = dayMeans$interval),"averageSteps"]
        }
    }
    return(completeData)
}

completeData <- patchNAs(completeData, dayMeans)
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
cdata <- completeData %>%
    group_by(date) %>%
    summarise(totalStepsTaken = sum(steps,na.rm = TRUE))

hist(cdata$totalStepsTaken,col = "blue",main = "Histogram of total steps taken per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/q3.4-1.png)<!-- -->

```r
mean(cdata$totalStepsTaken, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(cdata$totalStepsTaken, na.rm = TRUE)
```

```
## [1] 10766.19
```
Patching the missing values has alligned the median and the mean. Resulting in no SKU of the data.

# Question 4:
Are there differences in activity patterns between weekdays and weekends?

1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
wdata <- data %>%
    mutate(weekendFlag = case_when(
        weekdays(date) == "Sunday" ~ "Weekend",
        weekdays(date) == "Saturday" ~ "Weekend",
        TRUE ~ "Weekday"))

head(wdata)
```

```
##   steps       date interval weekendFlag
## 1    NA 2012-10-01        0     Weekday
## 2    NA 2012-10-01        5     Weekday
## 3    NA 2012-10-01       10     Weekday
## 4    NA 2012-10-01       15     Weekday
## 5    NA 2012-10-01       20     Weekday
## 6    NA 2012-10-01       25     Weekday
```

2) Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
awdata <- wdata %>%
    group_by(interval, weekendFlag) %>%
    summarise(steps = mean(steps,na.rm = TRUE))
ggplot(data = awdata, aes(x = interval, y = steps, colour = weekendFlag))+geom_line()+ facet_wrap(.~weekendFlag)
```

![](PA1_template_files/figure-html/q4.2-1.png)<!-- -->
