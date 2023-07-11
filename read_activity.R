#setwd("~/r-studio-test/R Studio/repdata_data_activity")
val <- read.csv("activity.csv")

StepPerDay <- c()
stepct <- 0
for(i in 2:nrow(val)){
  if(val$date[i] == val$date[i - 1]){
    if(is.na(val$step[i]) == FALSE){
    stepct <- stepct + val$step[i]
    }
    else{
      stepct <- stepct + 0
    }
  }
  else{
    StepPerDay <- c(StepPerDay, stepct)
    if(is.na(val$step[i]) == FALSE){
      stepct <- val$step[i]
    }
  }
}
StepPerDay <- c(StepPerDay, 0)

Dates <- c(val$date[1])
for(i in 2:nrow(val)){
  if(val$date[i] != val$date[i-1]){
    Dates <- c(Dates, val$date[i])
  }
}
plot(x = as.POSIXct(Dates), y = dfActivity$StepPerDay,
              xlab = "Dates for Each Element", type = "h",
              ylab = " Number of Steps Taken", main = "Number of Steps per Day",
              col = "blue")

library("ggplot2")
AllAverage <- c()
SumStep <- 0
for(i in 1:nrow(val)){
  if(is.na(val$steps[i]) == FALSE){
    SumStep = SumStep + as.numeric(val$steps[i])
    AllAverage <- c(AllAverage, SumStep/i)
  }
}
plot(x = as.POSIXct(val$date[!is.na(val$steps)]), y = AllAverage, type = "l",
     xlab = "Number of Steps Taken", ylab = "Date", 
     main = "Average Number of Steps Taken")

max(val$steps[!is.na(val$steps)])

removena <- na.omit(val)

StepPerDay1 <- c()
stepct1 <- 0
for(i in 2:nrow(removena)){
  if(removena$date[i] == removena$date[i - 1]){
    stepct1 <- stepct1 + removena$step[i]
  }
  else{
    StepPerDay1 <- c(StepPerDay1, stepct1)
      stepct1 <- removena$step[i]
  }
}
StepPerDay1 <- c(StepPerDay1, stepct1)

Dates1 <- c(removena$date[1])
for(i in 2:nrow(removena)){
  if(removena$date[i] != removena$date[i-1]){
    Dates1 <- c(Dates1, removena$date[i])
  }
}
plot(x = as.POSIXct(Dates1), y = StepPerDay1,
     xlab = "Dates for Each Element", type = "h",
     ylab = " Number of Steps Taken", main = "Number of Steps per Day")

weekday <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekdaysteps <- c()
weekendsteps <- c()
for(i in 1:nrow(removena)){
  if(weekdays(as.POSIXct(removena$date[i])) %in% weekday){
    weekdaysteps <- c(weekdaysteps, removena$steps[i])
  }
  else{
    weekendsteps <- c(weekendsteps, removena$steps[i])
  }
}

sumweekday <- 0
avgweekday <- c()
for(i in 1:length(weekdaysteps)){
  sumweekday <- sumweekday + as.numeric(weekdaysteps[i])
  avgweekday <- c(avgweekday, sumweekday/i)
}
sumweekend <- 0
avgweekend <- c()
for(i in 1:length(weekendsteps)){
  sumweekend <- sumweekend + as.numeric(weekendsteps[i])
  avgweekend <- c(avgweekend, sumweekend/i)
}

par(mfrow = c(2, 1))
plot(x = 1:length(avgweekday), y = avgweekday, type = "l", 
     xlab = "Number of Five-Minute Intervals", ylab = "Number fo Steps Taken",
     main = "Average Steps Taken on Weekdays")
plot(x = 1:length(avgweekend), y = avgweekend, type = "l",
     xlab = "Number of Five-Minute Intervals", ylab = "NUmber of Steps Taken", 
     main = "Average Steps Taken on Weekends")