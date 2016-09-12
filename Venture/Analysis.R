# TODO: Add comment
# 
# Author: kps3
###############################################################################

library(xlsx)
library(gmapsdistance)

#alldataSet = read.xlsx("All Trip Sheet.xlsx", sheetIndex=1, header=TRUE)
dataSet <- read.table("Sample Data Dec 15.csv", header=TRUE, sep=",")


address1 = URLencode("Webster Hall, 10038,, 125 East 11th Street, New York, NY 10003")
address2 = URLencode("The Manhattan at Times Square Hotel, 790 7th Avenue, New York, NY 10019")
address3 = URLencode("20 Russel St., New Haven, CT 06513")
address4 = URLencode("22 Peat Meadow Rd., New Haven, CT 06512")


results = gmapsdistance(origin = c(address1,address3),
		destination = c(address2,address4),
		combinations='pairwise',
		mode = "driving",
		traffic_model="optimistic",
		key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')


results = gmapsdistance(origin = c("Seattle+WA", "Miami+FL"),
		destination = c("Chicago+IL", "Philadelphia+PA"),
		mode = "bicycling",
		dep_date = "2017-08-16",
		dep_time = "20:40:00")


routes = dataSet$Routing.Details
#select the origins
origins = gsub("(.*PU: )(.*?)(;.*)","\\2",routes)
#select the destinations
destinations = gsub("(.*DO: )(.*?)(;.*)","\\2",routes)

#TODO check with rob that its ok to parse this as follows
#strip the comment before the comma by looking for the streat number
origins[-grep("[Aa]ir", origins)] = gsub("(^.*?, )([0-9].*)","\\2",origins[-grep("[Aa]ir", origins)])
destinations[-grep("[Aa]ir", destinations)] = gsub("(^.*?, )([0-9].*)","\\2",destinations[-grep("[Aa]ir", destinations)])

#HTML encode the origins and destinations
origins = sapply(origins, URLencode)
destinations = sapply(destinations, URLencode)

#select only the airports
inds = union(grep("[Aa]ir", origins),grep("[Aa]ir", destinations))
#durations = gmapsdistance(
#		origin = origins[inds],
#		destination = destinations[inds],
#		combinations='pairwise',
#		mode = "driving",
#		traffic_model="optimistic",
#		key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')


#save(durations, file="Durations.RData")
load(file="Durations.RData")
googleTime = durations$Time$Time/60
# add googleTime to data
data = cbind(dataSet[inds,], googleTime)
# remove NAs
data= na.omit(data)

data$Duration = as.numeric(strptime(data$Duration,"%H:%M")-strptime("00:00","%H:%M"))

data$PU.Time = as.numeric(strptime(data$PU.Time,"%H:%M %p") - strptime("00:00","%H:%M"))

DayOfWeek = weekdays(as.Date(data$PU.Date,"%m/%d/%Y"))

#excluding monday to avoid colinearity create day of week variables
week = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekData = data.frame(matrix(0, dim(data)[1],6))
colnames(weekData)= week
for(day in week){
	weekData[DayOfWeek==day, day]=1
}
data = cbind(data, weekData)

plot(density(data$Duration))
plot(density(data$googleTime))

plot(data$Duration, data$googleTime)
data = data[data$googleTime!=0,]
data = data[data$googleTime<200,]
plot(data$Duration, data$googleTime)

variables = c("PU.Time","googleTime", week)

z <- cor(data[,c("Duration",variables)])
require(lattice)
levelplot(z,scales=list(x=list(rot=90,cex=0.5),y=list(cex=0.5)),at=1:20/10-1,pretty=TRUE)


formula = paste("Duration",paste(variables,collapse = '+'),sep='~')
fit <- lm(formula, data=data)

summary(fit)

beta = as.matrix(as.numeric(fit$coefficients))

X <- as.matrix(cbind(1,data[,variables]))
yhat= X%*%beta

plot(data$Duration,type='l')
lines(yhat, col='red')
lines(data$googleTime, col='blue')
