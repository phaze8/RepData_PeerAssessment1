unzip("activity.zip")
act <- read.csv("activity.csv")
library(lubridate)
act$date <- ymd(as.character(act$date))
day.totals <- aggregate(act$steps, by=list(as.factor(act$date)), FUN=sum, na.rm=TRUE)
colnames(day.totals) <- c("date", "total.steps")
## Mean and Median
day.totals.summary <- data.frame(mean=mean(day.totals$total.steps, na.rm=TRUE), median=median(day.totals$total.steps, na.rm=TRUE))
## Plot of total steps/day hist
par(mfrow=c(1,1))
hist(day.totals$total.steps, xlab="Total Daily Steps", ylab="Freq", main="Histogram of Total Steps per Day", col="lightblue")
text(x=21000, y=25, labels=paste("Mean: ", as.character(as.integer(day.totals.summary$mean)), "\nMedian: ", as.character(as.integer(day.totals.summary$median))), col="purple")


## Plot of Avg Steps by Interval
interval.avg <- aggregate(act$steps, by=list(act$interval), FUN=mean, na.rm=TRUE)
colnames(interval.avg) <- c("interval", "Average.Steps")
plot(x=interval.avg$interval, y=interval.avg$Average.Steps, type="l", main="Average Steps by Interval", col="darkblue", lwd=3, ylab="Steps", xlab="Interval")
max.interval <- interval.avg[interval.avg$Average.Steps==max(interval.avg$Average.Steps),]
max.interval$interval

## Total number of NAs
sum(is.na(act$steps))

## Create new data set and set steps equal to the average for that interval
act2 <- act
for(i in 1:nrow(act2)){
  act2[i,1] <- ifelse(is.na(act2[i,1]), interval.avg[interval.avg$interval==act2[i,3], 2], act2[i,1])
}

day.totals2 <- aggregate(act2$steps, by=list(as.factor(act2$date)), FUN=sum, na.rm=TRUE)
colnames(day.totals2) <- c("date", "total.steps")
## Mean and Median
day.totals.summary2 <- data.frame(mean=mean(day.totals2$total.steps, na.rm=TRUE), median=median(day.totals2$total.steps, na.rm=TRUE))
## Plot of total steps/day hist
par(mfrow=c(2,1), mar=c(4,4,2,1))
hist(day.totals$total.steps, xlab="Total Daily Steps", ylab="Freq", main="Histogram of Total Steps per Day", col="lightblue")
text(x=21000, y=20, labels=paste("Mean: ", as.character(as.integer(day.totals.summary$mean)), "\nMedian: ", as.character(as.integer(day.totals.summary2$median))), col="purple")
hist(day.totals2$total.steps, xlab="Total Daily Steps", ylab="Freq", main="Histogram of Total Steps per Day (NAs imputed)", col="lightblue")
text(x=21000, y=25, labels=paste("Mean: ", as.character(as.integer(day.totals.summary2$mean)), "\nMedian: ", as.character(as.integer(day.totals.summary2$median))), col="purple")

##Create weekday factor column
library("plyr")
act3 <- act2
act3 <- mutate(act3, day.of.week=weekdays(date))
for(i in 1:nrow(act3)){
  if(length(grep("^S", act3[i,4])) > 0){
    act3[i,5] <- "Weekend"
  }
  else {
    act3[i,5] <- "Weekday"
  }
}
colnames(act3)[5] <- "day.type"
act3 <- mutate(act3, day.type=as.factor(day.type))
interval.avg3 <- aggregate(act3$steps, by=list(act3$interval, act3$day.type), FUN=mean, na.rm=TRUE)
##Plot
library(lattice)
colnames(interval.avg3) <- c("interval", "day.type", "Average.Steps")
xyplot(Average.Steps~interval|day.type, data=interval.avg3, type="l", layout=c(1,2), lwd=3, main="Average steps by day type")