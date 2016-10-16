

```r
---
title: "Reproducible research. Course Project 1"
author: "R. G."
date: "16 ottobre 2016"
output: html_document
keep_md: TRUE
---
```

```
## Error: <text>:9:0: unexpected end of input
## 7: ---
## 8: 
##   ^
```


```r
dataAct<-read.csv("C:/Users/guglielmo/Desktop/DATA SCIENCE/DC WORKING DIRECTORY/activity.csv", sep=",",header=TRUE)
str(dataAct)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
##Introduction
In this document you can find an analysis of the dataset called "Activity" that contains 17568 observation of 3 variables (steps, date, interval). The data are taken from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day.

##What is mean total number of steps taken per day?

```r
#mean
dataframeM<-data.frame(na.omit(aggregate(steps ~ date, dataAct,sum)))
mean0<-sum(dataframeM$steps)/53
#median
median0<-median(dataframeM$steps, na.rm=TRUE)
```
So, the mean of the total steps gropued by day is `mean0`, while the median is `median0`
The histogram of the total number of steps taken each day.

```r
dataframe1<-data.frame(aggregate(steps ~ date, dataAct, mean))
library(ggplot2) 
g<-ggplot(data = dataframe1, aes(x=steps))+geom_histogram(bins=53)
g
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
##What is the average daily activity pattern?

```r
dataframe2<-data.frame(na.omit(aggregate(steps ~ interval, dataAct,mean)))

ggplot( data = dataframe2, aes( interval, steps)) + geom_line()+ggtitle("Average daily activity pattern") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
max(dataframe2$steps)#that is interval 835
```

```
## [1] 206.1698
```
##Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
na_count <-sapply(dataAct, function(y) sum(length(which(is.na(dataAct)))))

na_count <- data.frame(na_count)

summary(dataAct)#the summary command confirms that there are 2304 rows with NA. All the NAs are in the steps column.
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
Therefore, there are 2304 rows with NAs.
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
median1<-median(dataAct$steps, na.rm=TRUE)
dataAct2<-dataAct 
dataAct2$steps[is.na(dataAct2$steps)] <-median1
```
I substituted the NAs with the median of steps.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#mean
dataframeMNA<-data.frame(aggregate(steps ~ date, dataAct2,sum))
meanNA<-sum(dataframeMNA$steps)/53
#median
medianNA<-median(dataframeMNA$steps, na.rm=TRUE)
#Histogram
dataframe3<-data.frame(aggregate(steps ~ date, dataAct2, mean))
g1<-ggplot(data = dataframe3, aes(x=steps))+geom_histogram(bins=53)
g1
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
The new mean is `meanNA` and the new median is `medianNA`.
##Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dataAct2$dayType<-weekdays(as.Date(dataAct2$date))
dataAct2$weekType<-as.factor(ifelse(dataAct2$dayType == "sabato"|dataAct2$dayType == "domenica", "weekend", "weekday"))  
```
2. Panel Plot

```r
dataframefinal<-data.frame(aggregate(steps ~ interval+weekType, dataAct2, mean))
g2<-ggplot(data = dataframefinal, aes( interval, steps)) + geom_line()+facet_grid(weekType~. )
g2
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
```

