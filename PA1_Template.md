---
title: "Reproducible Research: Assignment 1"
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
```


##    Code for reading in the dataset and/or processing the data
```{r}
actna<-read.csv("activity.csv")
actna$date<-as.Date(actna$date)
act<-actna[complete.cases(actna),]

```

##    Histogram of the total number of steps taken each day
```{r}
byday<-tapply(act$steps,act$date,sum)
hist(byday,main="Histogram of the numbers of steps taken each day", xlab="Steps by day")
```

##    Mean and median number of steps taken each day
```{r}
bydate<-group_by(act,date)  %>% mutate("meandate"=mean(steps)) %>% mutate("mediandate"=median(steps))
```

##    Time series plot of the average number of steps taken
```{r}
plot(bydate$date,bydate$meandate,type='l', main="Average of steps taken by day",xlab="Date",ylab="Average number of Steps")
```

##    The 5-minute interval that, on average, contains the maximum number of steps
```{r}
act<-group_by(act,interval) %>% mutate("meanint"=mean(steps))
max<-act[which.max(act$meanint),3][[1]]
```
The 5-minute interval that contains on average the maximum number of steps is: `r max`.


##    Code to describe and show a strategy for imputing missing data
To replace missing data with useful values, we first need to get the rows with missing values (in our dataset, only the "steps" colum has missing data). Then, we crate a temporary dataset with the matching intervals and mean number of steps per interval.  
This allows to match to each missing value the average number of steps for the corresponding interval.
```{r}
summary(actna)
ind<-which(is.na(actna[,1]))
agg<-act[which(!duplicated(act$interval)),c(3,4)]   
actna[ind,1]<-merge(actna,agg)[ind,4]
```

##    Histogram of the total number of steps taken each day after missing values are imputed
```{r}
hist(tapply(actna$steps,actna$date,sum),main="Histogram of the numbers of steps taken each day", xlab="Steps by day")
```

##    Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
actna$day<-weekdays(actna$date)

## Create a dataset with weekdays data (days in French)
week<-filter(actna,day=="lundi"|day=="mardi"|day=="mercredi"|day=="jeudi"|day=="vendredi")
week<-group_by(week,interval) %>% mutate("meanint"=mean(steps))

## Create a dataset with week-end data (days in French)
week_end<-filter(actna,day=="samedi"|day=="dimanche")
week_end<-group_by(week_end,interval) %>% mutate("meanint"=mean(steps))

par(mfrow=c(1,2))
plot(week$interval,week$meanint,type='l',main="Weekdays",xlab="5 min interval",ylab="Average number of steps")
plot(week_end$interval,week_end$meanint,type='l',main="Week-end",xlab="5 min interval",ylab="")
title("Average number of steps by 5 minutes interval", line = -1, outer = TRUE)
```