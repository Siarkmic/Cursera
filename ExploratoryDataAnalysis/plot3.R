


# Description: Measurements of electric power consumption in one household with 
# a one-minute sampling rate over a period of almost 4 years. Different electrical 
# quantities and some sub-metering values are available.

# Date: Date in format dd/mm/yyyy
# Time: time in format hh:mm:ss
# Global_active_power: household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage: minute-averaged voltage (in volt)
# Global_intensity: household global minute-averaged current intensity (in ampere)
# Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
# Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# install.packages("data.table")
library(data.table)

## download data
setwd("C:/Users/siarkmi2/Documents/GitHub/Cursera/ExploratoryDataAnalysis")
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fileName <- "powerConsumtion.zip"

if (!file.exists("fileName")) {
        download.file(url, fileName)
        unzip(fileName)
        file.remove(fileName)
}


## load data option nrows = 1000
powerConsumtion <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)


# Selecting adequate lines
powerConsumtion[powerConsumtion =="?" ] <- NA
powerConsumtion$Date <- as.Date(powerConsumtion$Date, format = "%d/%m/%Y")

subPC <- subset(powerConsumtion, Date >= "2007-02-01" & Date <= "2007-02-02") 
subPC$Global_active_power <- as.numeric(subPC$Global_active_power)

# NEW 
# Join date and time
subPC$DateTime <- as.POSIXct(strptime(paste(subPC$Date, subPC$Time, sep = " "),
                                               format = "%Y-%m-%d %H:%M:%S" )) 




# Graph
png(file ="plot3.png", width = 480, height = 480, units = "px")

plot(subPC$DateTime,
     subPC$Sub_metering_1,
     type = "l",
     xlab = "",
     ylab = "Global Active Power (kilowatts)"
     )
points(subPC$DateTime,
       type = "l",
       subPC$Sub_metering_2,
       col = "red")
points(subPC$DateTime,
       type = "l",
       subPC$Sub_metering_3,
       col = "blue")
legend("topright", col = c("black", "blue", "red"),
       legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), lty = 1)


dev.off() # Close the png file device

















