#Set Working Directory
setwd("C:/_MIROSLAV/POSAO/Coursera/MyRdirectory/5. Reproducible Research/W1")

#Install and Load required package for grouping data frame
install.packages("dplyr")
library(dplyr)

#Install and Load required package for ggplot2
install.packages("ggplot2")
library(ggplot2)

#Install and Load required package for ggplot2
install.packages("knitr")
library(knitr)

#########################################################
#Loading and preprocessing the data
#########################################################

dfActivityRaw <- read.csv("activity.csv")

#Deleting rows where steps are equals to NA
dfActivity <- dfActivityRaw[!is.na(dfActivityRaw$steps), ]





#########################################################
#What is mean total number of steps taken per day?
#########################################################

#Aggregating "steps" for each "date" - by calculating summmary of "steps"
dfActivityDay <- aggregate(steps ~ date, data = dfActivity, sum, na.rm = TRUE)


#Plotting
###########

hist(dfActivityDay$steps, main="Frequency of total number of steps taken each day", xlab = "Total number of steps taken each day", ylab = "Frequency", col="red")
rug(dfActivityDay$steps)
     
#Save the plot file as PNG file
dev.copy(png,"plot1.png", width=480, height=480)

#close device
dev.off()


#Calculating Mean & Median
############################
stepsMean <- mean(dfActivityDay$steps, na.rm = TRUE)
stepsMedian <- median(dfActivityDay$steps, na.rm = TRUE)





#########################################################
#What is the average daily activity pattern?
#########################################################

#Aggregating "steps" for each "interval" - by calculating mean of "steps"
dfActivityInterval <- aggregate(steps ~ interval, data = dfActivity, mean, na.rm = TRUE)

#Calculating "interval" for which "steps" have max value
dfActivityInterval$interval[which.max(dfActivityInterval$steps)]

#Verification
#max(dfActivityInterval$steps)
#dfActivityInterval[dfActivityInterval$interval == 835, ] 

#Plotting
###########

ggplot(dfActivityInterval, aes(interval, steps)) +
        geom_line(colour = "red", lwd = 1) +
        ggtitle("Number of steps by interval (average across all days)") +
        xlab("Interval") +
        ylab("Number of steps")

#Save the plot file as PNG file
dev.copy(png,"plot2.png", width=480, height=480)

#close device
dev.off()





#########################################################
#Imputing missing values
#########################################################


#Filling in missing values (NA) in "steps" with mean value accross all days for that 5-minute interval 
dfActivityFilled <- dfActivityRaw

numberOfstepsNA <- length(which(is.na(dfActivityFilled$steps)))
stepsNA <- which(is.na(dfActivityFilled$steps))


for (i in 1:numberOfstepsNA) {

        numberOfRow <- stepsNA[i]
        numberOfInterval <- dfActivityFilled$interval[numberOfRow]
        meanStepsForInterval <- dfActivityInterval$steps[dfActivityInterval$interval == numberOfInterval]
        dfActivityFilled$steps[numberOfRow] <- meanStepsForInterval
        
}


#What is mean total number of steps taken per day?
#########################################################

#Aggregating "steps" for each "date" - by calculating summmary of "steps"
dfActivityFilledDay <- aggregate(steps ~ date, data = dfActivityFilled, sum, na.rm = TRUE)

#Plotting
hist(dfActivityFilledDay$steps, main="Frequency of total number of steps taken each day", xlab = "Total number of steps taken each day", ylab = "Frequency", col="red")
rug(dfActivityFilledDay$steps)

#Save the plot file as PNG file
dev.copy(png,"plot3.png", width=480, height=480)

#close device
dev.off()


#Calculating Mean & Median
stepsFilledMean <- mean(dfActivityFilledDay$steps, na.rm = TRUE)
stepsFilledMedian <- median(dfActivityFilledDay$steps, na.rm = TRUE)

#Differences compared to non-filled Mean and Median
meanDiffPerc <- round(100*(stepsMean - stepsFilledMean)/ stepsMean, 4)
medianDiffPerc <- round(100*(stepsMedian - stepsFilledMedian)/ stepsMean, 4)



####################################################################################
#Are there differences in activity patterns between weekdays and weekends?
####################################################################################


#Converting column "date" from Factor to Date
dfActivityFilled$date <- as.Date(dfActivityFilled$date, format = "%Y-%m-%d")

#Creating new column "Day"
dfActivityFilled$day <-  factor(ifelse(weekdays(dfActivityFilled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))



#What is the average daily activity pattern?
#########################################################

#Aggregating "steps" for each "interval" and "day" - by calculating mean of "steps"
dfActivityFilledInterval <- aggregate(steps ~ interval + day, data = dfActivityFilled, mean, na.rm = TRUE)


#Plotting
###########

ggplot(dfActivityFilledInterval, aes(interval, steps)) +
        geom_line(colour = "red", lwd = 1) +
        facet_wrap(~day, ncol = 1) +
        ggtitle("Number of steps by interval (average across all days)") +
        xlab("Interval") +
        ylab("Number of steps")
        

#Save the plot file as PNG file
dev.copy(png,"plot4.png", width=480, height=480)

#close device
dev.off()

