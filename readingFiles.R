
# dowloading file from wbb

setwd("C:/Users/siarkmi2/Documents/GitHub/Cursera")

download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
              destfile = "./Downloads/communities.csv"
              )
comm.raw <- read.csv2("./Downloads/communities.csv", sep =",", header = TRUE )
View(comm.raw)

x <- subset(comm.raw, VAL==24, select = c(VAL, FES))

# XLSX

install.packages(xlsx)
library(xlsx)
download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx",
              destfile = "./Downloads/airQ.xlsx")
airQ.raw <- read.


# XML
library(XML)
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileURL, useInternalNodes = TRUE)
class(doc)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]][[3]]

#dostac sie do konkretnego node xml

xml.zip <- as.data.frame(xpathSApply(rootNode, "//zipcode", xmlValue))
colnames(xml.zip)[1] <- "zip.code"

x <-subset(xml.zip, zip.code == 21231, select = zip.code )

## Data table time
library(data.table)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile = "./Downloads/somedata.csv")
DT <- fread("./Downloads/somedata.csv")


system.time(replicate(1000,tapply(DT$pwgtp15,DT$SEX,mean))  )
#user  system elapsed 
#1.56    0.00    1.61 

#system.time(replicate(10,rowMeans(DT)[DT$SEX==1]))

system.time(replicate(1000,sapply(split(DT$pwgtp15,DT$SEX),mean)))
#user  system elapsed 
#1.35    0.00    1.40 


system.time(replicate(1000, DT[,mean(pwgtp15),by=SEX]))
#user  system elapsed 
#1.03    0.03    1.09 


system.time(replicate(1000, mean(DT$pwgtp15,by=DT$SEX)))
#user  system elapsed 
#0.05    0.00    0.05 

system.time(replicate(1000,mean(DT[DT$SEX==1,]$pwgtp15))) 
#user  system elapsed 
#21.42    0.03   21.81 

## Data from Web
library(XML)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
htmlCode <- htmlTreeParse(url, useInternalNodes = TRUE)

xpathSApply(htmlCode, "//title", xmlValue)
# or

htmlCode <- readLines(url)

nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

## data from fixed width file

x <- read.fwf(
        file=url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"),
        skip=4,
        widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4))

head(x)

sum(x$V4)



