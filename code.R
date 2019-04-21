# ****************************************************
## Loading and preprocessing the data
# ****************************************************

# Download zip file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists("activity.zip")) {
    download.file(url, "activity.zip", mode = "wb")
}

# Unzip zip file
unzip("activity.zip")

# Load the data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# Explore the data
dim(activity)
summary(activity)
str(activity)
head(activity)
tail(activity)

# ****************************************************
## What is mean total number of steps taken per day?
# ****************************************************

# Calculate the total number of steps taken per day
totalStepsPerDay <- aggregate(steps ~ date, activity, sum) 
head(totalStepsPerDay) # The default for aggregate is to ignore missing values in the given variables.

# Histogram of the total number of steps taken each day
ggplot(totalStepsPerDay, aes(x = steps)) + geom_histogram(bins = 10) + geom_vline(aes(xintercept = meanTotalStepsPerDay), colour="black") +
# Barplot of the total number of steps taken each day
ggplot(data=totalStepsPerDay, aes(x=date, y=steps)) + geom_bar(stat="identity")

#g <- ggplot(totalStepsPerDay, aes(x = steps)) + geom_histogram(bins = 10)
#g + geom_vline(aes(xintercept = meanTotalStepsPerDay), colour="black")

# The mean and median of the total number of steps taken per day
meanTotalStepsPerDay <- mean(totalStepsPerDay$steps, na.rm = TRUE)
meanTotalStepsPerDay
medianTotalStepsPerDay <-median(totalStepsPerDay$steps, na.rm = TRUE)
medianTotalStepsPerDay

# ****************************************************
## What is the average daily activity pattern?
# ****************************************************

# Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgStepsPerInterval <- aggregate(steps ~ interval, activity, mean)
ggplot(avgStepsPerInterval, aes(x = interval, y = steps)) + geom_line()
plot(x = avgStepsPerInterval$interval, y = avgStepsPerInterval$steps, type='l', 
     xlab = "Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern", col = "blue")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avgStepsPerInterval[which.max(avgStepsPerInterval$steps), ]$interval

# ****************************************************
## Imputing missing values
# ****************************************************

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
sum(is.na(activity))
colSums(is.na(activity))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


# Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity2<-activity
for(i in 1:nrow(activity2)){
    if(is.na(activity2[i,]$steps)){
        activity2[i,]$steps <- avgStepsPerInterval[avgStepsPerInterval$interval==activity2[i,]$interval,]$steps
    }
}

head(activity2)
tail(activity2)

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalStepsPerDay2 <- aggregate(steps ~ date, activity2, sum)
ggplot(totalStepsPerDay, aes(x = steps)) + geom_histogram(bins = 10)

meanTotalStepsPerDay2 <- mean(totalStepsPerDay2$steps, na.rm = TRUE)
meanTotalStepsPerDay2
medianTotalStepsPerDay2 <-median(totalStepsPerDay2$steps, na.rm = TRUE)
medianTotalStepsPerDay2

# ***************************************************************************
## Are there differences in activity patterns between weekdays and weekends?
# ***************************************************************************

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
activity2$day <- ifelse(as.POSIXlt(activity2$date)$wday%%6 == 0, "weekend", "weekday")
activity2$day <- factor(activity2$day,levels=c("weekday","weekend"))


# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). 

avgStepsPerInterval2 <- aggregate(steps~interval+day, activity2, mean)
library(lattice)
xyplot(steps~interval|factor(day), data=avgStepsPerInterval2, aspect=1/2, type="l")
