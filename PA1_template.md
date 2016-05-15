##Reproducible Research Project 1

###1. Loading and Preprocessing the Data
####Load the necessary packages and data.

```r
library(ggplot2)
library(lattice)

activityData <- read.csv("activity.csv")
```


###2. What is the mean total number of steps taken per day?
####For the analysis in this section, NA entries are ignored and discarded. 

```r
totalSteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(totalSteps, binwidth=1000, xlab="Total Number of Steps / Day", main ="Histogram of Total Number of Steps per day", fill=I("blue"), col=I("grey"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

####Calculate the mean and median of total steps per day.

```r
calculate <- function(steps) {
        c(MEAN = mean(steps), MEDIAN = median(steps))
}

calculate(totalSteps)
```

```
##     MEAN   MEDIAN 
##  9354.23 10395.00
```


###3. What is the average daily activity pattern?
####For the analysis in this section, NA entries are ignored and discarded.
####The mean is calculated for each 5 minute interval is calculated across all days.
####The mean for each 5 minute interval is then plotted to generate a daily activity pattern.

```r
averagePat <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval), mean, na.rm=TRUE)

ggplot(data=averagePat, aes(y=steps, x=interval)) +
        geom_line(color="blue") +
        xlab("5 Minute Intervals") +
        ylab("Average Steps") +
        ggtitle("Average Steps taken at 5 minute Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

####Which 5 minute interval, on average across all days in the dataset, contains the maximum number of steps?

```r
averagePat[which.max(averagePat$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


###4. Inputing Missing Values
####Fill in the missing values (NA) in the dataset using the mean of steps taken per day.
####activityData2 will be the dataset with the missing values filled. 

```r
activityData2 <- activityData
missingIndex <-  which(is.na(activityData$steps))

for (i in missingIndex) {
activityData2$steps[i] = averagePat[averagePat$interval == activityData2$interval[i], "steps"]
}
```
####Plot the histogram of Total Number of Steps per day with the filled dataset.

```r
totalSteps2 <- tapply(activityData2$steps, activityData2$date, FUN=sum, na.rm=TRUE)
qplot(totalSteps2, binwidth=1000, xlab="Total Number of Steps / Day", main ="Histogram of Total Number of Steps per day", fill=I("blue"), col=I("grey"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

####Calculate the mean and median of total steps per day.

```r
calculate(totalSteps2)
```

```
##     MEAN   MEDIAN 
## 10766.19 10766.19
```


###5. Differences in Activity Patterns between Weekdays and Weekends
####Classify each entry as Weekend or Weekday.

```r
days <- weekdays(as.Date(activityData2$date))

activityData2$day_type <- ifelse(days == "Saturday" | days == "Sunday", "Weekend", "Weekday")
```
####Plot the daily activity pattern for both for comparison.

```r
averagePat2<- aggregate(activityData2$steps, 
                       by=list(activityData2$interval,
                       activityData2$day_type),mean)

names(averagePat2) <- c("interval","day_type","steps")

xyplot(steps~interval | day_type, averagePat2, type="l",
       layout=c(1,2), xlab="5 Minute Intervals", ylab = "Average Steps", main = "Daily Activity Comparison Weekend vs Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

####Calculate the mean and median for both for comparison.

```r
tapply(averagePat2$steps,averagePat2$day_type, calculate)
```

```
## $Weekday
##     MEAN   MEDIAN 
## 35.61058 25.80314 
## 
## $Weekend
##     MEAN   MEDIAN 
## 42.36640 32.33962
```



