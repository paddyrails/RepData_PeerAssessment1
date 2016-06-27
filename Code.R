setwd("/Users/padmanabhanpillai/Git/RepData_PeerAssessment1")
activity <- read.csv("activity.csv")

#Question 1
TotalStepsPerDay <- aggregate(steps ~ date, activity, sum)
hist(TotalStepsPerDay$steps)
mean(TotalStepsPerDay$steps)

#Question 2
MeanStepsPerInterval <- aggregate(steps ~ interval, activity, mean)
plot(MeanStepsPerInterval$interval, MeanStepsPerInterval$steps, type = "l", xlab = "5-minute intervals", ylab = "Average of steps in 2 months", main = "5-minute intervals Vs Avg number of steps taken")

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
sum(is.na(activity$steps))
# Replace na with Mean of that interval 
imputed_activity <- transform(activity, steps = ifelse(is.na(activity$steps), MeanStepsPerInterval$steps[match(activity$interval, MeanStepsPerInterval$interval)], activity$steps))

#Question 3
activity$date <- as.Date(activity$date, "%Y-%m-%d")