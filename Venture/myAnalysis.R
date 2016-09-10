# TODO: Add comment
# 
# Author: kps3 (mod. bswiswa)
###############################################################################

# library(xlsx) optional package to read xlsx format. CSV used in this case

# gmaps distance package for time estimates
library(gmapsdistance)
# get data
dataSet = read.table("Sample Data Dec 15.csv", header=TRUE, sep=",", as.is = TRUE)
# look at data
str(dataSet)


address1 = URLencode("Webster Hall, 10038,, 125 East 11th Street, New York, NY 10003")
address2 = URLencode("The Manhattan at Times Square Hotel, 790 7th Avenue, New York, NY 10019")
address3 = URLencode("20 Russel St., New Haven, CT 06513")
address4 = URLencode("22 Peat Meadow Rd., New Haven, CT 06512")


# results = gmapsdistance(origin = c(address1,address3),
# 		destination = c(address2,address4),
# 		combinations='pairwise',
# 		mode = "driving",
# 		traffic_model="optimistic",
# 		key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')
# look at sample results
results
str(results)
# results = gmapsdistance(origin = c("Seattle+WA", "Miami+FL"),
# 		destination = c("Chicago+IL", "Philadelphia+PA"),
# 		mode = "bicycling",
# 		dep_date = "2017-08-16",
# 		dep_time = "20:40:00")


routes = dataSet$Routing.Details
#select the origins
origins = gsub("(.*PU: )(.*?)(;.*)","\\2",routes)
#select the destinations
destinations = gsub("(.*DO: )(.*?)(;.*)","\\2",routes)

#TODO check with rob that its ok to parse this as follows
#strip the comment before the comma by looking for the street number
origins[-grep("[Aa]ir", origins)] = gsub("(^.*?, )([0-9].*)","\\2",origins[-grep("[Aa]ir", origins)])
# eg "Home, 149 Winchester Ave, New Haven, CT " loses the initial "Home"
destinations[-grep("[Aa]ir", destinations)] = gsub("(^.*?, )([0-9].*)","\\2",destinations[-grep("[Aa]ir", destinations)])
# same with destinations

#HTML encode the origins and destinations (more suitable to gmapsdistance)
origins = sapply(origins, URLencode)
destinations = sapply(destinations, URLencode)

#select only the airports (returns all indexes with an airport origin or destination)
inds = union(grep("[Aa]ir", origins),grep("[Aa]ir", destinations)) #count = 285 
durations = gmapsdistance(
	origin = origins[inds],
	destination = destinations[inds],
	combinations='pairwise',
	mode = "driving",
	traffic_model="optimistic",
	key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')

#save(durations, file="Durations.RData")
load(file="Durations.RData")
gmapsTime = durations$Time$Time/60 # count = 285
gmapsDistance <- durations$Distance$Distance*0.00062 # convert to miles
# get only origins and destinations we are interested in
air.origin <- sapply(origins[inds], URLdecode)  #named char vectors
air.dest <- sapply(destinations[inds], URLdecode) #named char vectors
# get actual durations
actualTimeChar <- dataSet$Duration[inds] #char vector
# convert to numeric minutes
actualTime <- sapply(actualTimeChar, 
                     function(x){ frags <- as.numeric(unlist((strsplit(x,":"))))
                        return(frags[1]*60 + frags[2])})
serviceType <- dataSet$Service.Type[inds]
driver <- dataSet$Driver[inds]
pickup.day <- sapply(dataSet$PU.Date[inds], function(x){ weekdays(as.Date(x,'%m/%d/%y'))})
timeStamps <- paste(dataSet$PU.Date[inds],dataSet$PU.Time[inds], sep = " ")
pickup.timeObj <- as.POSIXlt(timeStamps,format="%m/%d/%Y %I:%M %p")
# remove NAs
driveTimeMinutes <- driveTimeMinutes[!is.na(driveTimeMinutes)] # count = 220
# get times of day
##############################
# processing needed
# need variable day - which contains the day of the week of the pickup
# need variable actualTime which has the actual MINUTES the journey took
# need to separate all my calculations by the Service.Type



# create our data frame
data <- data.frame(pickup.timeObj, pickup.day, driver,serviceType,
                   air.origin, air.dest, actualTime, gmapsTime, gmapsDistance)
names(data) <- c("PU.timeObj", "day", "driver", "serviceType",
                 "origin", "dest", "actualTime", "gTime", "gDistance")
#remove NAs
data <- data[complete.cases(data),]
# remove rows with gTime and gDistance == 0
data <- data[data$gTime != 0 | data$gDistance != 0,]


# set parameters for plot appearance
par(mar=c(5,5,4,1))
# compare actual and predicted times
plot(data$gTime,data$actualTime,
     main="Comparison of Actual vs Predicted\n Driving Times",
     xlab="Predicted Time(min)",
     ylab="Actual Time(min)",
     pch=19,
     col="blue")

# compare pick ups by day
days <- factor(data$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

barplot(table(days), col=unique(days),
        main = "Total Number of Pickups\nBy Days of the Week",
        xlab = "Day",
        ylab = "Number")
## serviceType
table(data$serviceType)

for(i in 1:dim(data)[1]){
  if(data$serviceType[i] == "Round Trip") data$gTime[i] <- 2*data$gTime[i]
}

# compare predicted to actual driving times
plot(data$gTime,data$actualTime,
     main="Comparison of Actual vs Predicted\n Driving Times",
     xlab="Predicted Time(min)",
     ylab="Actual Time(min)",
     pch=19,
     col="blue")
# fit lines
fit <- lm(data$actualTime ~ data$gTime)
abline(fit, col="red", lwd=2)

# plot residuals
plot(data$gTime, resid(fit), 
          ylab="Residuals", xlab="Predicted Time", 
          main="Residuals From Linear Model",
          pch=19,
          col="blue") 

#plot zero line
abline(0,0, col="red")

# try different traffic model - more pessimistic or best guess?
# expand on data modeled for? or focus on airport pickups?
# difftime function strtime
# collinearity, happens when you have too many variables in your fit
# bin time of day into morning, afternoon, evening

