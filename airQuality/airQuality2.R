## This first line will likely take a few seconds. Be patient!
setwd("C:/Users/siarkmi2/Documents/GitHub/Cursera/airQuality")
NEI.raw <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

NEI <- NEI.raw

head(NEI)
head(SCC)
NEI$Pollutant <- as.factor(NEI$Pollutant)
summary(NEI$Pollutant)

library(data.table)
NEI.raw <- data.table(NEI.raw)

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from all 
## sources for each of the years 1999, 2002, 2005, and 2008.

png("plot1.png")
NEI.sum <- NEI.raw[, sum(Emissions), by = year]

plot(NEI.sum$year, NEI.sum$V1, 
     type = "l",
     col = "3",
     lwd = "2",
     xlab = "Year",
     ylab = "Amount of PM2.5 emitted, in tons",
     main = "Total emissions from PM2.5"
     )
fit1 <- lm (V1 ~ year, data = NEI.sum) 
abline(fit1, lty = "dashed")
dev.off()

## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
## from 1999 to 2008? Use the base plotting system to make a plot answering this question.
png("plot2.png")
NEI.sum <- NEI.raw[fips == "24510", sum(Emissions), by = year]

plot(NEI.sum$year, NEI.sum$V1, 
     type = "l",
     col = "3",
     lwd = "2",
     xlab = "Year",
     ylab = "Amount of PM2.5 emitted, in tons",
     main = "Total emissions from PM2.5 in the Baltimore City, Maryland "
)
fit1 <- lm (V1 ~ year, data = NEI.sum) 
abline(fit1, lty = "dashed")
dev.off()
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? \
## Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)
NEI.sum <- NEI.raw[fips == "24510", sum(Emissions), by = .(year, type) ]
NEI.sum
fit1 <- lm (V1 ~ year+ type, data = NEI.sum) 

ggplot(NEI.sum, aes(year , V1, color = type)) +
        geom_line()+
        geom_smooth(method='lm', span = 0.1, level=0.20)+
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
        labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))
ggsave("plot3.png")       


## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

NEI.raw
SCC <- data.table(SCC)
SCC
# Subset coal combustion related NEI data
combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustionRelated & coalRelated)

SCC.sub <- SCC[ coalCombustion]
NEI.sub <- NEI.raw[NEI.raw$SCC %in% SCC.sub$SCC]

NEI.sum <- NEI.sub[, sum(Emissions), by = year]

ggplot(NEI.sum, aes(factor(year), V1, fill=factor(year)))+
        geom_bar(stat = "identity")+
        guides(fill=FALSE)+ # remove legend
        labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)"))+
        labs(title=expression("PM"[2.5]*" Emissions from coal combustion-related sources"))

ggsave("plot4.png")
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

NEI.sum <- NEI.raw[fips == '24510' & type == 'ON-ROAD', sum(Emissions), by = year]
NEI.sum

ggplot(NEI.sum, aes(factor(year), V1, fill=factor(year)))+
        geom_bar(stat = "identity")+
        guides(fill=FALSE)+ # remove legend
        labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)"))+
        labs(title=expression("PM"[2.5]*" emissions from motor vehicle sources"))
ggsave("plot5.png")

## Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?

NEI.sum.B <- NEI.raw[fips == '24510' & type == 'ON-ROAD', sum(Emissions), by = year]
NEI.sum.C <- NEI.raw[fips == '06037' & type == 'ON-ROAD', sum(Emissions), by = year]

NEI.sum.B$City <- "BaltimoreCity"
NEI.sum.C$City <- "California"

NEI.sum <- rbind(NEI.sum.B, NEI.sum.C)
NEI.sum
# Generate the graph in the same directory as the source code

ggplot(NEI.sum, aes(factor(year), V1, fill=factor(year)))+
        geom_bar(stat = "identity")+
        guides(fill=FALSE)+ # remove legend
        facet_grid(. ~ City)+
        labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)"))+
        labs(title=expression("PM"[2.5]*" emissions from motor vehicle sources"))

ggsave("plot6.png")


