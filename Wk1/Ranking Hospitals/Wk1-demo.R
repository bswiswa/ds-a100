#set working directory
setwd("Wk1")

#import data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hosp.data <- read.csv("hospital-data.csv")

#peek at data
head(outcome)
View(outcome)
str(outcome)

head(hosp.data)
View(hosp.data)
str(hosp.data)

#plot the 30-day mortality rates for heart attack
outcome[,11] <- as.numeric(outcome[,11]) #convert from char to numeric
hist(outcome[,11], col = rainbow(12),
     main = "30-day mortality rates for heart attack",
     xlab = "Mortality rates",
     ylab = "Number of hospitals",
     ylim = c(0,800) )
