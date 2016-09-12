# TODO: Add comment
# 
# Author: kps3 (mod. bswiswa)
###############################################################################

library(xlsx)
# get data
dataSet <-  read.xlsx2("V2 Trip Data Sheet.xlsx", sheetIndex=1, stringsAsFactors = FALSE)
# gmaps distance package for time estimates
library(gmapsdistance)
# look at data
str(dataSet)

# address1 <- URLencode("Webster Hall, 10038,, 125 East 11th Street, New York, NY 10003")
# address2 <- URLencode("The Manhattan at Times Square Hotel, 790 7th Avenue, New York, NY 10019")
# address3 <- URLencode("20 Russel St., New Haven, CT 06513")
# address4 <- URLencode("22 Peat Meadow Rd., New Haven, CT 06512")


# results = gmapsdistance(origin = c(address1,address3),
# 		destination = c(address2,address4),
# 		combinations='pairwise',
# 		mode = "driving",
# 		traffic_model="optimistic",
# 		key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')
# look at sample results

# results = gmapsdistance(origin = c("Seattle+WA", "Miami+FL"),
# 		destination = c("Chicago+IL", "Philadelphia+PA"),
# 		mode = "bicycling",
# 		dep_date = "2017-08-16",
# 		dep_time = "20:40:00")


routes <-  dataSet$Routing.Details
#select the origins
origins <-  gsub("(.*PU: )(.*?)(;.*)","\\2",routes)
#select the destinations
destinations <-  gsub("(.*DO: )(.*?)(;.*)","\\2",routes)

#TODO check with rob that its ok to parse this as follows
#strip the comment before the comma by looking for the street number
origins[-grep("[Aa]ir", origins)] <- gsub("(^.*?, )([0-9].*)","\\2",origins[-grep("[Aa]ir", origins)])
# eg "Home, 149 Winchester Ave, New Haven, CT " loses the initial "Home"
destinations[-grep("[Aa]ir", destinations)] <- gsub("(^.*?, )([0-9].*)","\\2",destinations[-grep("[Aa]ir", destinations)])
# same with destinations
stop.wait.inds <- grep(" ST:| WT:", routes) #about 7355 rides
#HTML encode the origins and destinations (more suitable to gmapsdistance)
originsURL <- sapply(origins, URLencode)
destinationsURL <- sapply(destinations, URLencode)

#select only the airports (returns all indexes with an airport origin or destination)
all.inds <- union(grep("[Aa]ir", origins),grep("[Aa]ir", destinations)) #count = 10607
# get only indices without stops(ST:) or waits(WT:) 
inds <- setdiff(all.inds, stop.wait.inds)   # 10261
#get only the first 250 points
# durations <- gmapsdistance(
# 	origin = originsURL[inds[5000:5250]],
# 	destination = destinationsURL[inds[5000:5250]],
# 	combinations='pairwise',
# 	mode = "driving",
# 	traffic_model="optimistic",
# 	key='AIzaSyBcdRj2y_EncY3t-ypdJXb0QCkpDTMRfJw')
# # ran durations at 12:47pm 9/11/16
# save(durations, file="Durations.RData")
# load(file="Durations.RData")
# # run again with more points (try spreading out better)
# durations2 <- gmapsdistance(
#   origin = originsURL[inds[9000:9250]],
#   destination = destinationsURL[inds[9000:9250]],
#   combinations='pairwise',
#   mode = "driving",
#   traffic_model="optimistic",
#   key='AIzaSyBcdRj2y_EncY3t-ypdJXb0QCkpDTMRfJw')
# # ran durations at 11:10pm 9/10/16
# save(durations2, file="Durations2.RData")
# load(file="Durations2.RData")

# combine the two lists
test.durations <- mapply(rbind,durations,durations2, SIMPLIFY = FALSE)
test.inds <- inds[c(5000:5250, 9000:9250)]
gmapsTime = test.durations$Time$Time/60 # count = 500
gmapsDistance <- test.durations$Distance$Distance*0.00062 # convert to miles
# get only origins and destinations we are interested in and tested for
air.origin <- origins[test.inds]  #named char vectors
air.dest <- destinations[test.inds] #named char vectors
# get actual durations
actualTimeChar <- dataSet$Duration[test.inds] #char vector
# convert to numeric minutes
actualTime <- sapply(actualTimeChar, 
                     function(x){ frags <- as.numeric(unlist((strsplit(x,":"))))
                        return(frags[1]*60 + frags[2])})
#serviceType <- dataSet$Service.Type[test.inds] # no longer needed
driver <- dataSet$Driver[test.inds]
timeStamps <- paste(dataSet$PU.Date[test.inds],dataSet$PU.Time[test.inds], sep = " ")
pickup.date <- as.POSIXlt(timeStamps,format="%m/%d/%Y %I:%M %p")

# create our data frame
to.airport <- grepl("[Aa]ir", air.dest)
data <- data.frame(pickup.date, driver,
                   air.origin, air.dest, actualTime, gmapsTime, gmapsDistance,to.airport, stringsAsFactors = FALSE)

names(data) <- c("pickup.date","driver",
                 "origin", "dest", "actualTime", "gTime", "gDistance", "to.airport")

#remove NAs
data <- data[complete.cases(data),]
# remove rows with gTime and gDistance == 0
data <- data[data$gTime > 0 & data$gDistance > 0,]

# create a weekdays data frame to add to data but remove one day to avoid collinearity
pickup.day <- sapply(data$pickup.date, function(x){ weekdays(as.Date(x, tz="EST"))})
week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
week.data.frame <- data.frame(matrix(0, dim(data)[1], 6))
names(week.data.frame) <- week
for(day in week){
  week.data.frame[pickup.day == day, day] <- 1
}

# split the day into 3 time periods - morning(0000-1159), afternoon(1200-1759), 
# evening(1800-1159)
# only specify 2 to avoid collinearity
timePeriod <- c("morning", "afternoon")
timePeriod.frame <- data.frame(matrix(0,dim(data)[1], 2))
names(timePeriod.frame) <- timePeriod
morning <- 0:11
afternoon <- 12:17
time.list <- list(morning, afternoon)
names(time.list) <- timePeriod
for(time in timePeriod){ 
  timePeriod.frame[as.POSIXlt(data$pickup.date)$hour %in% time.list[time][[1]], time] <- 1
}
# join the data frames
all.data <- cbind(data,week.data.frame)
all.data <- cbind(all.data, timePeriod.frame)
# deal with outliers
for(i in 1:dim(all.data)[1]){
  if(all.data$gTime[i] > 3*data$actualTime[i]) all.data$gTime[i] <- data$actualTime[i]
}
# plot time density distributions
par.default <- par()
par(mfrow=c(2,1), mar=c(5,5,2,1))
plot(density(all.data$actualTime),
     main="Density plot for Actual Ride Durations",
     xlab = "Duration(min)")
plot(density(all.data$gTime),
     main="Density plot for Predicted Ride Durations",
     xlab = "Duration(min)")

# reset parameters for plot appearance
suppressWarnings(par(par.default))
par(mar=c(5,5,4,1))
# compare predicted to actual driving times
plot(all.data$gTime,all.data$actualTime,
     main="Comparison of Actual vs Predicted\n Driving Times",
     xlab="Predicted Time(min)",
     ylab="Actual Time(min)",
     pch=19,
     col="blue")
# fit lines
fit1 <- lm(actualTime ~ gTime, data = all.data)
abline(fit1, col="red", lwd=2)

# plot residuals
plot(all.data$gTime, resid(fit), 
     ylab="Residuals", xlab="Predicted Time", 
     main="Residuals From Linear Model",
     pch=19,
     col="blue") 

#plot zero line
abline(0,0, col="red")

# compare pick ups by day
days <- factor(pickup.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

barplot(table(days), col=unique(days),
        main = "Total Number of Pickups\nBy Days of the Week",
        xlab = "Day",
        ylab = "Number")



#visualize correlations
variables <- c("gTime", "gDistance", week, timePeriod)
z <- cor(all.data[,c("actualTime",variables)])
require(lattice)
levelplot(z,scales=list(x=list(rot=90,cex=0.5),y=list(cex=0.5)),at=1:20/10-1,pretty=TRUE)
# regression
formula = paste("actualTime",paste(variables,collapse = '+'),sep='~')
fit <- lm(formula, data=all.data)

summary(fit)
# get coefficients for each variable in linear model
print(fit$coefficients)
beta = as.matrix(as.numeric(fit$coefficients))
print(beta)
# arrange elements for matrix dot product (easier)
X <- as.matrix(cbind(1,all.data[,variables]))
yhat= X%*%beta

# make a function that predicts the time
getorigin <- function(){
  or <- readline(prompt="Enter an origin address: ")
  if(is.na(or)) or <- getorigin()
  return(as.character(or))
}
getdestination <- function(){
  dest <- readline(prompt="Enter your destination address: ")
  if(is.na(dest)) dest <- getdestination()
  return(as.character(dest))
}
getdate <- function(){
  date <- readline(prompt = "Enter the date and approximate time of travel eg. 12/16/2016 9:30 AM :")
  if(is.na(date)) date <- getdate()
  date <- as.character(date)
  return(as.POSIXlt(date, format ="%m/%d/%Y %I:%M %p"))
}
gettimeperiod <- function(h){
  if(h %in% 0:11) return("morning")
  if(h %in% 12:17) return("afternoon")
  return("evening")
}

predict <- function(){
  origin <- getorigin()
  print(origin)
  dest <- getdestination()
  print(dest)
  date <- getdate()
  orURL <- URLencode(origin)
  destURL <- URLencode(dest)
  day <- weekdays(date)
  print(day)
  time.period <- gettimeperiod(date$hour)
  print(time.period)
  x <- c(0,0)
  if(time.period == "morning") x <- c(1,0)
  if(time.period == "afternoon") x <- c(0,1)
  y <- c(0,0,0,0,0,0)
  names(y) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  if(day != "Sunday") y[day] = 1
  print(y)
  tripduration <- gmapsdistance(
  	origin = orURL,
  	destination = destURL,
  	combinations='pairwise',
  	mode = "driving",
  	traffic_model="optimistic",
  	key='AIzaSyBcdRj2y_EncY3t-ypdJXb0QCkpDTMRfJw')
  print(tripduration)
print(beta)
  gtime <- tripduration$Time/60
  gdist <- tripduration$Distance*0.00062
  
  var <- c(gtime, gdist, y, x)
  print(var)
  thisX <- t(as.matrix(c(1,var)))
  print("dim(thisX)")
  print(dim(thisX))
  print("dim(beta)")
  print(dim(beta))
  cat("The predicted duration is ")
  return(as.numeric(thisX%*%beta/60))
}

plot(all.data$actualTime,type='l',
     main="Comparison of actual times and model predictions",
     ylab="Time(min)")
lines(yhat, col='red')
lines(all.data$gTime, col='blue')