Tracking personal movement with activity monitoring devices
===========================================================

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

First of all we read in the data from the csv file (located in the working directory), and make sure the columns are transformed to the correct format.

```r
monitoring <- read.csv("activity.csv", header=TRUE, na.strings=c("NA"), colClasses =c("numeric","character","numeric"))
monitoring$date <- as.Date(monitoring$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?

First of all, to understand the distribution of the number of steps the subject took each day, we construct a histogram.

Note: we are ignoring the missing values in the dataset.


```r
totalsteps <- tapply(monitoring$steps,monitoring$date,sum,na.rm = TRUE)
hist(totalsteps,xlab="Steps taken by day",ylab="Frequency",main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


```r
mean <- mean(totalsteps,na.rm = TRUE)
median <- median(totalsteps,na.rm = TRUE)
```

The mean of the total number of steps is 9354.2295 and the median is 1.0395 &times; 10<sup>4</sup>, reflecting the highest frequency of total steps in the [10,000; 15,000] bracket with a asymmetric distribution weighted to the left.


## What is the average daily activity pattern?

We create a line chart of the average number of steps taken in each 5-minute interval (averaged across all days). 


```r
averagesteps <- tapply(monitoring$steps,monitoring$interval,mean,na.rm = TRUE)
intervals <- seq(0, 55, by=5)
for (i in 1:23) {
 intervals <- c(intervals,seq(i*100,i*100+55,by=5)) 
}
plot(averagesteps ~ intervals, monitoring,xlab="5-minute intervals",type="l", ylab="Average number of steps taken")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


```r
max <- max(averagesteps)
top <- intervals[which(averagesteps == max(averagesteps))]
```
The 5-minute interval which contains the most steps on average across all days is interval 835 with an average of 206.1698 steps.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

The number of missing values in the data table by column is listed in below table:


```r
library(xtable)
```

```
## Error: there is no package called 'xtable'
```

```r
xt <- colSums(is.na(monitoring))
print(xt,type="html")
```

```
##    steps     date interval 
##     2304        0        0
```

```r
highest <- max(xt)
```

Hence, the total number of rows with missing values is 2304.

To counteract a potential bias of results, we will fill in the missing values in the dataset with the average number of steps during the same 5-minute interval on other days.To preserve the original data, this adapted dataset will be stored under a different name.


```r
monitoring2 <- monitoring
for (i in 1:nrow(monitoring2)) {
 if (is.na(monitoring2$steps[i])){
   monitoring2$steps[i]<-averagesteps[which(intervals==monitoring2$interval[i])]
 }
}
```

Now we repeat the exploratory analysis we did in the beginning, to see if the results differ.



```r
totalsteps2 <- tapply(monitoring2$steps,monitoring2$date,sum,na.rm = TRUE)
hist(totalsteps2,xlab="Steps taken by day",ylab="Frequency",main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
mean2 <- mean(totalsteps2,na.rm = TRUE)
median2 <- median(totalsteps2,na.rm = TRUE)
```

The histogram has a more symmetric distribution compared to the original one. The mean of the total number of steps is now 1.0766 &times; 10<sup>4</sup> and the median is 1.0766 &times; 10<sup>4</sup>. The equality of the mean and median again indicated symmetry of the distribution. This suggests that the initial asymmetry was caused by missing values.



## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.
