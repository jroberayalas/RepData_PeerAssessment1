# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Read data
data <- read.csv("activity.csv")

# Transform date column to date format using lubridate library
library(lubridate)
data$date <- ymd(data$date)

# Transform dataset to tbl_df format using the dplyr library
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- tbl_df(data)
```

## What is mean total number of steps taken per day?

```r
# Make a histogram of the total number of steps taken each day
library(ggplot2)
qplot(x = steps, data = data, binwidth = 30)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median total number of steps taken per day
# Mean
with(data, mean(steps, na.rm = TRUE))
```

```
## [1] 37.3826
```

```r
# Median
with(data, median(steps, na.rm = TRUE))
```

```
## [1] 0
```

## What is the average daily activity pattern?

```r
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Group data by interval and summarise the mean of the steps
average.steps <- data %>%
    group_by(interval) %>%
    summarise(mean(steps, na.rm = TRUE))
names(average.steps) <- c("interval", "ave.step")

# Plot
q <- qplot(x = interval, y = ave.step, data = average.steps, geom = "line",
           xlab = "Interval", ylab = "Average number of steps")
q
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max.steps <- which.max(average.steps[ , 2])
max.interval <- average.steps[ , 1][max.steps]

q + geom_vline(aes(xintercept = max.interval), col = "red", size = 1)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-2.png) 

The 835-minute interval contains the maximum number of steps on average across all the days in the dataset.

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

From the summary, we can observe that there are 2304 missing values (NAs) within the **steps** variable. The other two variables (**date** and **interval**) have no NAs.


```r
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```



## Are there differences in activity patterns between weekdays and weekends?
