---
title: "Activity"
author: "Ty Swarts"
date: "August 9, 2016"
output: pdf_document
---

```{r}
library(dplyr)
library(lubridate)
```

## Homework Week 2

Loading and preprocessing the data

```{r, echo=TRUE}
setwd("~/Coursera Data Class/Reproducible Data Analysis/Homework week 2")
data<-read.csv("activity.csv", na.strings = "NA", header = TRUE)
```

#What is mean total number of steps taken per day??---------------------------------------------
#Daily Total
```{r, echo=TRUE}
Dtot<-aggregate(data[,1], list(data$date), sum)
Dtot

```
#Histogram
```{r, echo=TRUE}
hist(Dtot$x)
```
#Mean
```{r, echo=TRUE}
Dmean<-aggregate(data[,1], list(data$date), mean)
Dmean
```
#Median
```{r, echo=TRUE}
Dmedian<-aggregate(data[,1], list(data$date), median)
Dmedian
```

#What is the average daily activity pattern?


```{r, echo=TRUE}
data_na <- filter(data,!is.na(data$steps))
data_int <- aggregate(data=data_na,steps ~ interval,mean)


plot(data_int$interval,data_int$steps,type="l",xlab="Interval (5 min)", ylab="Mean steps taken")
```

#Imputing missing values----------------------------------------------------------------

#Caculate total number of NAs

```{r, echo=TRUE}
summary(data)

summary(data)

data_rep <- data
data_na_num <- is.na(data_rep$steps)
data_mean_rep <- tapply(data_rep$steps,data_rep$interval,mean,na.rm=T)
data_rep$steps[data_na_num] <- data_mean_rep[as.character(data_rep$interval[data_na_num])]
data_comp <- tapply(data_rep$steps,data_rep$date,sum)

hist(data_comp, xlab="Steaps Taken", main = "Steps taken a Day")
```

#Are there differences in activity patterns between weekdays and weekends?---------------------
```{r, echo=TRUE}
data_rep$date<-ymd(data_rep$date)

days<-weekdays(data_rep$date)
```
#Assign weekdays to weekdays
```{r, echo=TRUE}
days<-replace(days, days=="Monday", "Weekday")
days<-replace(days, days=="Tuesday", "Weekday")
days<-replace(days, days=="Wednesday", "Weekday")
days<-replace(days, days=="Thursday", "Weekday")
days<-replace(days, days=="Friday", "Weekday")
days<-replace(days, days=="Saturday", "Weekend")
days<-replace(days, days=="Sunday", "Weekend")
data_rep$days<-days
```
#filter Subsets
```{r, echo=TRUE}
weekend<-filter(data_rep,days=="Weekend")
weekday<-filter(data_rep,days=="Weekday")
weekend <- aggregate(weekend$steps,list(weekend$interval, weekend$days),sum)
weekday <- aggregate(weekday$steps,list(weekday$interval, weekday$days),sum)
```
#Plot Graphs
```{r, echo=TRUE}
plot(weekday$Group.1,weekday$x, col="blue", type = "l", xlab = "Step", ylab = "Interval")
lines(weekend$Group.1,weekend$x, col="red", type = "l")
legend(1800, 9000, c("Weekday","Weekend"), lty=c(1,1), col=c("blue","red"))
```
