---
title: "ReproducibleResearch_Week2_Assignment"
output: html_document
---

```{r}
knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/Joshua/Downloads/repdata_data_activity")
library(dplyr)
library(ggplot2)
activity <- read.csv("activity.csv")
```

## R Markdown


```{r}
summary(activity)
```
```{r}
str(activity)
```
```{r}
head(activity)
```
```{r}
steps.complete <- na.omit(activity)
steps.day <- group_by(steps.complete, date)
steps.day <- summarize(steps.day, steps=sum(steps))
summary(steps.day)
```

## Plot - Histogram of total number of steps taken each day

```{r}
qplot(steps, data=steps.day)
```

## Mean value

```{r}
mean(steps.day$steps)
```

## Median value

```{r}
median(steps.day$steps)
```

## Time series plot of the average number of steps taken

```{r}
steps.int <- group_by(steps.complete, interval)
steps.int <- summarize(steps.int, steps=mean(steps))
ggplot(steps.int, aes(interval, steps)) + geom_line()
```

## 5-minute interval on average contains maximum number of steps

```{r}
steps.int[steps.int$steps==max(steps.int$steps),]
```

## Code to describe method for imputing missing value data

```{r}
nrow(activity)-nrow(steps.complete)
```

## Replace missing values by merging mean values for intervals(steps.int)
## across all days with the original data (activity),
## then create new data set.

```{r}
names(steps.int)[2] <- "mean.steps"
steps.impute <- merge(activity, steps.int)
steps.impute$steps[is.na(steps.impute$steps)] <- steps.impute$mean.steps[is.na(steps.impute$steps)]
```

## Create histogram of the new data set with 
## total steps taken per day.

```{r}
steps.day.imp <- group_by(steps.impute, date)
steps.day.imp <- summarize(steps.day.imp, steps=sum(steps))
qplot(steps, data=steps.day.imp)
```

## Mean value of above

```{r}
mean(steps.day.imp$steps)
```

## Median value of above

```{r}
median(steps.day.imp$steps)
```

## Panel plot describing average steps taken over 5 minute interval
## on weekdays vs. weekends

```{r}
steps.impute$dayofweek <- weekdays(as.Date(steps.impute$date))
steps.impute$weekend <-as.factor(steps.impute$dayofweek=="Saturday"|steps.impute$dayofweek=="Sunday")
levels(steps.impute$weekend) <- c("Weekday", "Weekend")

## create data frames for weekdays and weekends

steps.weekday <- steps.impute[steps.impute$weekend=="Weekday",]
steps.weekend <- steps.impute[steps.impute$weekend=="Weekend",]

## Mean values for time intervals

steps.int.weekday <- group_by(steps.weekday, interval)
steps.int.weekday <- summarize(steps.int.weekday, steps=mean(steps))
steps.int.weekday$weekend <- "Weekday"
steps.int.weekend <- group_by(steps.weekend, interval)
steps.int.weekend <- summarize(steps.int.weekend, steps=mean(steps))
steps.int.weekend$weekend <- "Weekend"

## rbind the values and plot results

steps.int <- rbind(steps.int.weekday, steps.int.weekend)
steps.int$weekend <- as.factor(steps.int$weekend)
ggplot(steps.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```




