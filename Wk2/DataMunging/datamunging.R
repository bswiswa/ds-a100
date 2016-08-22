getwd()
setwd("Documents/a100-dev/ds-a100")
dir()
dir.exists("DataMunging")
if(!dir.exists("DataMunging")) dir.create("DataMunging")
setwd("./DataMunging/")
getwd()
?file.download
file.URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(file.URL, destfile = "IdahoHousingData.csv")
dir()
var1 <- sample(1:5)
var2 <- sample(6:10)
var3 <- sample(11:15)

set.seed(20000)
x <- data.frame(var1, var2, var3)
x
x[sample(1:5),]
x["var2"]
x$var2[c(1,3)] <- NA
x["var2"]
x
x[1,]
x[1:3,2:3]
x[-c(4,5), -1]
col2 <- x$var2
col2
col2 > 5
#get first column less than 3 and third column greater than 11
x[x$var1 < 3 & x$var3 > 11, ]
#arrange your data
set.seed(1333)
z <- sample(1:5) 
z
#sort returns sorted data
sort(z)
#order returns indices of the sort
order(z)
z[order(z)]
#order can be used to sort
x[order(x[,1]),]
y <- cbind(col1 = x[,1], col2 = LETTERS[sample(1:5)], col3 = x[,3], col4 = c(0,1,0,3,2))
set.seed(999)
y
y <- as.data.frame(y)
y
names
?order
y[order(y[["col4"]]),]
y[order(y[["col4"]], y[["col2"]]),]
?order
y[["col4"]][2] <- 0
y[["col4"]][2]
y[order(y[["col4"]]),]
y[order(y[["col4"]], y[["col2"]]),]
y
mydata <- read.csv("IdahoHousingData.csv")
head(mydata)
str(mydata)
head(mydata, n = 2)
#need a data dictionary


agricLogical <- mydata$ACR == 3 & mydata$AGS == 6
mydata[agricLogical,]
agr <- which(agricLogical == T )
head(agr)
subdata <- mydata[agr,]
dim(subdata)
head(mydata)
str(mydata)
mydata[,1:15]
file.URL2 <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(file.URL2, destfile = "BaltRest.csv")
baltrest <- read.csv("BaltRest.csv")
head(baltrest, 3)
tail(baltrest,3)
str(baltrest)
dim(baltrest)
summary(baltrest)
table(baltrest$name)
str(baltrest)
quantile(baltrest$councilDistrict)
quantile(baltrest$councilDistrict, probs = c(0.4, 0.4, 0.8))
colSums(is.na(baltrest))
sum(baltrest$zipCode == 21212 | baltrest$zipCode == 21213)
head(baltrest[baltrest$zipCode == 21212 | baltrest$zipCode == 21213,], 15)
dim(baltrest[baltrest$zipCode == 21212 | baltrest$zipCode == 21213,])
zips <- which(baltrest$zipCode %in% c(21212, 21213))
nearme <- baltrest$neighborhood %in% c("Roland Park", "Homeland")
nearme
nrow(baltrest)
baltrest$nearme <- baltrest$neighborhood %in% c("Roland Park", "Homeland")
head(baltrest, 3)
baltrest[baltrest$nearme,]
summary(baltrest$zipCode)
baltrest$invZip <- ifelse(baltrest$zipCode < 0, T, F)
head(baltrest,3)
head(baltrest[baltrest$invZip == F,])
baltrest$zipGroups <- cut(baltrest$zipCode, quantile(baltrest$zipCode), include.lowest = F)
head(baltrest, 3)
table(baltrest$zipGroups)
baltrest$zcf <- as.factor(baltrest$zipCode)
install.packages("Hmisc")
require(Hmisc)
x <- c(1:20)
x2 <- cut2(x, 4)
x2
?cut2
