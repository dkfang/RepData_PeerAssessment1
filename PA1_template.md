# Reproducible Research: Peer Assessment 1
## week 2 assignment
## Name: DongKai Fang
## Loading and preprocessing the data

```r
# First unzip and load in data 
rm(list = ls())
unzip("activity.zip")
mydata = read.csv('activity.csv', header = T, sep = ",", na.strings = "NA")

# Converting the “date” variable to a Date classe
# and set the “interval” variable to a factor:
mydata$date = as.Date(mydata$date, "%Y-%m-%d")
mydata$interval <- as.factor(mydata$interval)
```

## What is mean total number of steps taken per day?

```r
# excluding missing values
NA_i <- is.na(as.character(mydata$steps))
data_wo_NA <- mydata[!NA_i,]
# head(data_wo_NA, 10)

#Splits the data per date, and calculate the total number of steps per day
splitDay = split(data_wo_NA, data_wo_NA$date)
totalSteps = sapply(splitDay, function(x) sum(x$steps))
hist(totalSteps,main="Histogram of Frequency of TotalSteps per day", breaks = 15, col = "blue")
```

![](PA1_template_files/figure-html/mean total number of steps per day-1.png)<!-- -->

```r
# Calculate and report the mean and median total number of steps taken per day
steps_perDay <- aggregate(steps ~ date, data = data_wo_NA, sum)
#Adding column names
colnames(steps_perDay) <- c("date", "steps")
# mean total number of steps taken per day
mean(steps_perDay$steps)
```

```
## [1] 10766.19
```

```r
# median total number of steps taken per day
median(steps_perDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
##### 1. Make a histogram of the total number of steps taken each day

```r
#Calculating the average
steps_perInterval <- aggregate(data_wo_NA$steps, by=list(interval=data_wo_NA$interval), FUN=mean)

#Adding columns names
colnames(steps_perInterval) <- c("interval", "average_steps")

#ploting the average daily activity pattern 
plot(as.integer(levels(steps_perInterval$interval)), steps_perInterval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="red")
```

![](PA1_template_files/figure-html/average daily activity pattern-1.png)<!-- -->

##### 2. 5-minute interval, on average across all the days in the dataset,

```r
# The maximum number of average steps
max_steps <- max(steps_perInterval$average_steps)
max_steps
```

```
## [1] 206.1698
```

```r
# The 5-minute interval that contains the maximum number of steps
intervale_max_steps<-steps_perInterval[which.max(steps_perInterval$average_steps),]$interval
intervale_max_steps
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

## Imputing missing values
##### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# Calculate total number of missing values for the “steps” variable
x = sum(is.na(as.character(mydata$steps)))

# Calculate total number of missing values for the “date” variable
y = sum(is.na(as.character(mydata$date)))

# Calculate total number of missing values for the “interval” variable
z = sum(is.na(as.character(mydata$interval)))

# Total number of missing values for all three variables
x+y+z
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# finding NAs index
NA_i <- which(is.na(as.character(mydata$steps)))
complete_data <- mydata

# filling missing values using the mean for that 5-minute interval
complete_data[NA_i,]$steps <- unlist(lapply(NA_i, FUN=function(NA_i){
      steps_perInterval[mydata[NA_i,]$interval == steps_perInterval$interval,]$average_steps
      }))
```

##### 4. Making a histogram of the total number of steps taken each day for the complete dataset:

```r
#Creating a data frame with the steps taken for each day
steps_eachDay_complete <- aggregate(steps ~ date, data = complete_data, sum)
#Adding column names
colnames(steps_eachDay_complete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(steps_eachDay_complete$steps), breaks = 15, col = "blue", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
##### 1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
# Creating a factor variable "days"to store the day of the week:
complete_data$day <- as.factor(weekdays(complete_data$date))

# Creating a logical variable "weekday" (weekday or weekend) :
complete_data$WEorWD = ifelse(weekdays(complete_data$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")
# table(complete_data$WEorWD)

#Calculating the average number of steps for weekdays
weekdays_data <- complete_data[complete_data$WEorWD=="Weekday",]
steps_perInterval_weekdays <- aggregate(weekdays_data$steps,
                                        by=list(interval=weekdays_data$interval), FUN=mean)

#Calculating the average number of steps for weekend
weekends_data <- complete_data[complete_data$WEorWD=="Weekend",]
steps_perInterval_weekends <- aggregate(weekends_data$steps,
                                        by=list(interval=weekends_data$interval), FUN=mean)


#Adding columns names
colnames(steps_perInterval_weekdays) <- c("interval", "average_steps")
colnames(steps_perInterval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_perInterval_weekdays$day <- "Weekday"
steps_perInterval_weekends$day <- "Weekend"

#Merging the two data sets
week_data <- rbind(steps_perInterval_weekends, steps_perInterval_weekdays)
# str(week_data)
#Converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)
# str(week_data)
```


##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
#Making the plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#There are some differences between the average number of steps between weekdays and weekends. For instance, it appears that the user started a bit later on weekend mornings and tend to do smaller numbers on weekend mornings.
```








