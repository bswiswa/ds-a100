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


results = gmapsdistance(origin = c(address1,address3),
		destination = c(address2,address4),
		combinations='pairwise',
		mode = "driving",
		traffic_model="optimistic",
		key='AIzaSyAkkAciTuCFe9tKgQOk4y4R4T6V3UccowQ')
# look at sample results
results
str(results)
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
driveTimeMinutes = durations$Time$Time/60




