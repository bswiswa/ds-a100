# URL for GDP and rank of countries
fileUrl1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

##  Url for educational data of the countries
fileUrl2<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

# download the files
download.file(fileUrl1, destfile = "GDP.csv")
download.file(fileUrl2, destfile = "Education.csv")
getwd()
dir()
# practice with merging
set.seed(11111)
df1 <- data.frame(var1 = c(1:10), var2 = rep(3,10), var3 = LETTERS[1:10] )
df2 <- data.frame(var10 = c(1:10), var2 = rnorm(10), var33 = LETTERS[1:10])
df3 <- data.frame(var1 = c(1:10), var2 = rnorm(10), var3 = LETTERS[1:10])
df1
df2
df2$var2[c(3,5)] <- NA 
df3$var3[c(6,10)] <- NA
df2
df3
class(df3$var3)
?merge
merge1 <- merge(df1,df2)
merge2 <- merge(df1,df2, by.x = "var1", by.y = "var1")
merge1
merge2
merge2 <- merge(df1,df2, by.x = c("var1", "var3"), by.y = c("var10", "var33"))
# end of merge lesson

#continue with cleaning data
gdp <- read.csv("GDP.csv")
education <- read.csv("Education.csv")

head(gdp)
str(gdp)
head(education)
str(education)

neededColumns <- c(1,2,4,5)
removeRows <- 1:4
newNames <- c("Country Code", "Ranking", "Country Name", "Economy (millions USD)")
# remove unnecessary rows and columns
clean.gdp <- gdp[-(removeRows),neededColumns]
View(clean.gdp)
# give the columns the right name
names(clean.gdp) <- newNames
# remove the rows with no ranking
clean.gdp <- clean.gdp[1:190,]
View(clean.gdp)
# reset the row names
row.names(clean.gdp) <- NULL

#merge with education
mergeEduGDP <- merge(clean.gdp,education, by.x = "Country Code", by.y = "CountryCode")
View(mergeEduGDP)
# sort countries with decreasing Ranking
mergeEduGDP <- mergeEduGDP[order(mergeEduGDP$Ranking, decreasing = TRUE),]
View(mergeEduGDP)
# fails because the Ranking values are factors
class(mergeEduGDP$Ranking)
# make them numeric BUT you have to make them character first
# because factors are handled differently in R so you make
# them strings first
mergeEduGDP$NumericRank <- as.numeric(as.character(mergeEduGDP$Ranking))
# rerun the sort
mergeEduGDP <- mergeEduGDP[order(mergeEduGDP$NumericRank, decreasing = TRUE),]
# reset the row names
row.names(mergeEduGDP) <- NULL
View(mergeEduGDP)
