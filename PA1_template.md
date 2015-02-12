# Reproducible Research: Peer Assessment 1
Gerard Lopez  
Sunday, February 15, 2015  



```r
library("knitr")
opts_chunk$set(echo=TRUE)
```

## Loading and preprocessing the data


```r
library("lattice")

unzip("activity.zip", overwrite=TRUE)
stepData <- read.csv("activity.csv", colClasses=c('integer','character', 'integer'))
stepData$timeInterval <- as.POSIXct(strptime(formatC(stepData$interval, width = 4, format="d", flag ="0"), "%H%M"))
```

Data was read in using read.csv and a new datetime variable was created to translate interval variable into a time that R could more easily process.


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
StepsByDay <- by(stepData$steps, stepData$date, FUN=sum)

hist(StepsByDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


2. Calculate and report the mean and median total number of steps taken per day


```r
StepsByDayMean <- mean(StepsByDay, na.rm = TRUE)
StepsByDayMedian <- median(StepsByDay, na.rm = TRUE)
```

The mean total number of steps taken per day is 1.0766189\times 10^{4}.

The median total number of steps taken per day is 10765.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepsByInterval<- aggregate(stepData$steps, list(timeInterval=stepData$timeInterval), mean, na.rm= TRUE)

#set x limits
beginning <- format(as.POSIXct(paste(Sys.Date(),"00:00")), format="%Y-%m-%d %H:%M")
ending <- format((as.POSIXct(paste(Sys.Date(),"00:00")) + 24*60*60), format="%Y-%m-%d %H:%M")

xyplot( x ~ timeInterval, data=StepsByInterval, type = "l"
        , xlab = "Time", ylab = "Average # of Steps"
        , xlim=c(as.POSIXct(beginning),as.POSIXct(ending))
        , ylim=c(0,225)
        , scales = list(tck=c(1,0),
                        x=list(tick.number = 24, cex=0.8, rot=90, format="%H:%M"),
                        y=list(tick.number = 5))
        , main = "Average Number of Steps During 5 Minute Intervals Over 24 Hours"
        )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#
```



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
temp <- StepsByInterval[ StepsByInterval$x == max(StepsByInterval$x, na.rm=TRUE), ]
MaxStepInterval = strftime(temp$timeInterval, "%H:%M")

rm(temp)
```

The 5-minute interval on average across all the days in the dataset which contains the maximum number of steps is 08:35. 


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
rowsWithNAs <- nrow(stepData[!complete.cases(stepData),])
```

The number of rows with NA missing values is 2304.


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# For each thing, make some kind of aggregate calculation
        #StepsByInterval already does this 

# Merge the aggregate number back into the original data as a new imputation column based on a link between individual thing instances and the aggregate thing instance

ImputedData <- merge(stepData, StepsByInterval, by = c("timeInterval"))
ImputedData$steps = ifelse(is.na(ImputedData$steps), ImputedData$x, ImputedData$steps)
# ImputedData <- ImputedData[order(ImputedData$date,ImputedData$interval),]


# Get rid of the imputation column to tidy up.
ImputedData$x <- NULL 
```

Missing values in the data set are imputed from the average number of steps for all days at the interval that is missing.  So, for example, if a specific day is missing a measurement at the 01:05 AM interval, then that gap is filled with the average number of steps from all days a that time.



3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# ImputedData is that dataset.
```
A new data set was created called ImputedData.


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
StepsByDayImputed <- by(ImputedData$steps, ImputedData$date, FUN=sum)

hist(StepsByDayImputed)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
StepsByDayImputedMean <- mean(StepsByDayImputed, na.rm = FALSE)
StepsByDayImputedMedian <- median(StepsByDayImputed, na.rm = FALSE)
```

The mean total number of steps taken per day in the imputed data is 1.0766189\times 10^{4}.

The median total number of steps taken per day in the imputed data is 1.0766189\times 10^{4}.

These values are fairly similar to the estimates from the first part of this assignment.



## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
ImputedData$weekday = factor(ifelse(weekdays(as.Date(ImputedData$date)) %in% c("Saturday","Sunday"), "weekend", "weekday"))
```

A new factor variable was created.


2. A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
xyplot(ImputedData$steps ~ ImputedData$timeInterval | ImputedData$weekday, layout = c(1,2), type = "l"
        , xlab = "Interval", ylab = "Number of Steps"
         , xlim=c(as.POSIXct(beginning),as.POSIXct(ending))
        , scales = list(tck=c(1,0),
                        x=list(tick.number = 24, cex=0.8, rot=90, format="%H:%M"),
                        y=list(tick.number = 5))
        , main = "Average Number of Steps During 5 Minute Intervals Over 24 Hours")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
