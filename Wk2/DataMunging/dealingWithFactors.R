# generate table of MCDONALD'S and SUBWAY restaurants listing them by their zipcodes

# get data
baltrest <- read.csv("BaltRest.csv")
# look at it
head(baltrest, 7)
summary(baltrest)
str(baltrest$name)
# shows that restaurant names are factors, this will be important soon
str(baltrest$zipCode)
# zipcode is an integer which we expect

# group conditions for subsetting
macD <- which(baltrest$name == "MCDONALD'S")
subW <- which(baltrest$name == "SUBWAY")
macD.OR.subW <- baltrest[c(macD, subW), c("name", "zipCode")] #subsetting - select the rows of McD and Subway,
# and only show the name and zipcodes

#now try to get the table of McDonald's or Subways using our subset
table(macD.OR.subW$zipCode, macD.OR.subW$name)
# fails - it appears that all the restaurants are returned but we only want two
# this is because the restaurant names are factors and even though we only specify two,
# all the other levels of factors are returned as well
# SOLUTION -  need to convert the name column of macD.OR.subW to character  
macD.OR.subW$name <- as.character(macD.OR.subW$name)
# try again, BOOM, it works
table(macD.OR.subW$zipCode, macD.OR.subW$name)


