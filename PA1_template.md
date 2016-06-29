# Reproducible Research - Course Project 1
Jeremy Pachtinger  
June 27, 2016  


##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This project makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This project performs some basic explanatory analysis and addresses the following questions:  
  
 - What is mean total number of steps taken per day?  
 - What is the average daily activity pattern?  
 - What is the impact of missing values if imputed on the results?  
 - Are there differences in activity patterns between weekdays and weekends?  
  

##Loading and preprocessing the data

The first step is to read in the data, creating a secondary dataset without any records that have missing values.


```r
## Assume dataset is in working directory
activity<-read.csv("activity.csv")
##convert date field to date format
activity$date<-as.POSIXct(activity$date)    
## Create dataset with no NA's
actNoNA<-activity[complete.cases(activity),]
```

###What is mean total number of steps taken per day?

Using the clean ( ie no missing values) we do the following:  
  
 - Calculate the total number of steps taken per day  
 - Make a histogram of the total number of steps taken each day  
 - Calculate and report the mean and median of the total number of steps taken per day  
 

```r
## Calculate total steps per day
totsteps<-rowsum(actNoNA,actNoNA$date)[,1:2]
totsteps$date<-as.POSIXct(row.names(totsteps))
##Create Histogram
hist(totsteps$steps,breaks=30,xlab = "Steps", main="Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/initial plots & analysis-1.png)<!-- -->

Next we show the mean and median of the total number of steps per day

```r
meanmed<-cbind(mean(totsteps$steps) , median(totsteps$steps))
colnames(meanmed)<-c("Mean","Median")
print(meanmed,row.names=FALSE)
```

```
##          Mean Median
## [1,] 10766.19  10765
```
 

###What is the average daily activity pattern?  

We show a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
## Calculate average steps per interval
avgintsteps<-aggregate(actNoNA,by=list(actNoNA$interval),mean)
avgintsteps<-avgintsteps[,c(2,4)]
##Create time series plot
plot(avgintsteps$steps~avgintsteps$interval,type="l",xlab="Interval",ylab="Steps",main="Average Steps by Interval")
```

![](PA1_template_files/figure-html/time series-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(avgintsteps[which.max(avgintsteps$steps),],row.names=FALSE)
```

```
##     steps interval
##  206.1698      835
```

###Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is:

```r
print(nrow(activity)-nrow(actNoNA),row.names=FALSE)
```

```
## [1] 2304
```

Use the mean of each 5-minute interval to replace any missing values and create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
## create a dataset of the missing values
actNA<-activity[!complete.cases(activity),]
##Subset the NoNA dataset to the intervals in the NA dataset
actNoNAint<-subset(actNoNA,actNoNA$interval %in% actNA$interval)
##reset steps value in actNA from int to num
actNA$steps<-as.numeric(actNA$steps)
##reset interval value in avgintsteps from num to int
avgintsteps$interval=as.integer(avgintsteps$interval)
#Merge the datasets
actOK<-merge(actNA,avgintsteps,by="interval",all.x = TRUE)
##drop the steps.x
actOK$steps.x<-NULL
#rename the steps.y as steps
names(actOK)[names(actOK) == 'steps.y'] <- 'steps'
#attach the actOK dataset back to NoNA dataset
actallOk<-rbind(actNoNA,actOK)
```

We now make a histogram of the total number of steps taken each day using the new imputed dataset and Calculate and report the mean and median total number of steps taken per day.  

What impact has the imputation had on the data?  
  
 - Do these values differ from the estimates from the first part of the assignment?  
 - What is the impact of imputing missing data on the estimates of the total daily number of steps?
 

```r
totstepsall<-rowsum(actallOk,actallOk$date)[,1:2]
##Create Histogram and compare to prior
par(mfrow=c(1,2))
hist(totstepsall$steps,breaks=30,xlab = "Steps", ylim=c(0,20),main="Histogram of Total Steps per Day  \n (NAs replaced with mean of each interval)")
hist(totsteps$steps,breaks=30,xlab = "Steps",ylim=c(0,20), main="Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/histogram of imputed data-1.png)<!-- -->

```r
meanmedall<-cbind(mean(totstepsall$steps) , median(totstepsall$steps))
colnames(meanmedall)<-c("Mean.Imputed.NA","Median.Imputed.NA")
print(meanmedall,row.names=FALSE)
```

```
##      Mean.Imputed.NA Median.Imputed.NA
## [1,]        10766.19          10766.19
```

```r
print(meanmed,row.names=FALSE)
```

```
##          Mean Median
## [1,] 10766.19  10765
```

###Are there differences in activity patterns between weekdays and weekends?

Using the dataset with the filled-in missing values we create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  

We make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
#add weekday/weekend field
actallOk$wkDay<-ifelse( weekdays(actallOk$date) %in% "Saturday" | weekdays(actallOk$date) %in% "Sunday" ,"weekend","weekday")
#aggregate to interval by wkDay
actallwk<-actallOk[,-2] #drop date
require(plyr)
```

```
## Loading required package: plyr
```

```r
actallwk<-ddply(actallwk,.(interval,wkDay),colwise(mean))

#create panel plot
require(lattice)
```

```
## Loading required package: lattice
```

```r
 xyplot(actallwk$steps~actallwk$interval|actallwk$wkDay, 
   	main="Average Steps per Interval",
    xlab="Interval",ylab="Number of Steps",
    layout=c(1,2),type="l")
```

![](PA1_template_files/figure-html/weekday/end-1.png)<!-- -->


