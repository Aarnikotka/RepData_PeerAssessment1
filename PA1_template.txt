# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

It is assumed that files are downloaded and unzipped into your working directory. The next block of code reads the data and converts dates into Date format.


```r
my_data<-read.csv('activity.csv')
my_data$date<-as.Date(my_data$date)
```

## What is mean total number of steps taken per day?

First create a vector of total number of steps taken per day then plot a histogramm.


```r
steps_tot<-tapply(my_data$steps,my_data$date,FUN = sum,na.rm=TRUE)
hist(steps_tot,xlab='Steps number',main='Total number of steps taken each day',col='grey')
```

![](PA1_template_files/figure-html/steps_per_day_hist-1.png)<!-- -->

Evaluate mean and median of the total number of steps taken each day.


```r
steps_mm<-c(mean(steps_tot),median(steps_tot))
names(steps_mm)<-c('steps_mean','steps_median')
steps_mm
```

```
##   steps_mean steps_median 
##      9354.23     10395.00
```

## What is the average daily activity pattern?

Create a vector of average number of steps in each 5-minute interval then make a plot.


```r
steps_ts<-tapply(my_data$steps,my_data$interval,FUN = mean,na.rm=TRUE)
plot(names(steps_ts),steps_ts,type='l',lwd=2,col='blue',xlab='Interval',ylab='Steps number')
title(main='Time series plot of the 5-min intervals and the average steps number')
```

![](PA1_template_files/figure-html/step_av_5_min-1.png)<!-- -->

Find max average number of steps and the 5-min interval it belongs to


```r
steps_max<-c(max(steps_ts),as.numeric(which.max(steps_ts)))
steps_max<-c(steps_max,as.numeric(names(steps_ts)[steps_max[2]]))
names(steps_max)<-c('max_average_steps','interval_number','interval')
steps_max
```

```
## max_average_steps   interval_number          interval 
##          206.1698          104.0000          835.0000
```


## Imputing missing values

Start with the report of NA's


```r
summary(my_data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

Change NA's with mean values taken by interval. Substitute new values in a new data.frame. As **steps_ts** already contains all the necessary information, use it in a loop


```r
my_data_noNA<-my_data
my_vec<-matrix(c(as.numeric(names(steps_ts)),steps_ts),nrow=length(steps_ts))
for (i in 1:dim(my_vec)[1]) my_data_noNA[is.na(my_data_noNA$steps) & my_data_noNA$interval==my_vec[i,1],1]=my_vec[i,2]
head(my_data_noNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Create a new histogram


```r
steps_tot_noNA<-tapply(my_data_noNA$steps,my_data_noNA$date,FUN = sum)
hist(steps_tot_noNA,xlab='Steps number',main='Total number of steps taken each day',col='grey')
```

![](PA1_template_files/figure-html/steps_per_day_hist_noNA-1.png)<!-- -->

Evaluate mean and median of the total number of steps taken each day with NA being substituted by mean values


```r
steps_mm_noNA<-c(mean(steps_tot_noNA),median(steps_tot_noNA))
names(steps_mm_noNA)<-c('steps_mean','steps_median')
steps_mm_noNA
```

```
##   steps_mean steps_median 
##     10766.19     10766.19
```

As a result, we can see that substituting missing values lead to increase of median and mean estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor viriable indicating weekday or weekend. Have to use lubridate package, because weekdays() returns values which are not in english


```r
library(lubridate)
weekday_num<-wday(my_data_noNA$date)
weekday_name<-rep(NA,dim(my_data_noNA)[1])
weekday_name[weekday_num==1 | weekday_num==7]='weekend'
weekday_name[is.na(weekday_name)]='weekday'
my_data_noNA$weekdays<-weekday_name
```

Create a panel plot using lattice with mean steps in interval group by weekdays


```r
library(lattice)
my_data_noNA_day<-my_data_noNA[my_data_noNA$weekdays=='weekday',]
my_data_noNA_end<-my_data_noNA[my_data_noNA$weekdays=='weekend',]
steps_end<-tapply(my_data_noNA_end$steps,my_data_noNA_end$interval,FUN = mean)
steps_day<-tapply(my_data_noNA_day$steps,my_data_noNA_day$interval,FUN = mean)
my_new_data<-data.frame(c(steps_day,steps_end),check.names=FALSE)
names(my_new_data)<-'mean_steps'
my_new_data$weekdays<-c(rep('weekday',length(steps_day)),rep('weekend',length(steps_end)))
my_new_data$interval<-c(as.numeric(names(steps_day)),as.numeric(names(steps_end)))
xyplot(mean_steps~interval | weekdays, data=my_new_data,layout=c(1,2),type='l',main='Panel plot of average steps taken',lwd=2,col=c('blue','blue'),xlab='Interval',ylab='Average number of steps')
```

![](PA1_template_files/figure-html/plot_steps_by_weekdays-1.png)<!-- -->

