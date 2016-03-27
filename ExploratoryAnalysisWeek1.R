temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)

unlink(temp)file <- unzip(temp)

# read only selected data. Note: this will skip the header.
selData <- read.table(text = grep("^[1,2]/2/2007", readLines(file), value = TRUE), sep = ";")
# extract the header
headerData <- read.table(file, nrows = 1, sep = ";",stringsAsFactors = FALSE)
# Add the header to the selData
names(selData) <- headerData

# convert the first column from char to date. 
#selData$Date <- as.Date(selData$Date, "%d/%m/%Y")
# convert the time column from factor to time. 
#selData$Time <- strptime(selData$Time,"%H:%M:%S")

#----------------------- Create Plot 1.png --------------------------------------------
# Set to display one graph. 
par(mfrow = c(1,1),mar= c(4,4,2,1) )
#  Draw the histograph.
hist(selData$Global_active_power,col = "red", main ="Global Active Power", xlab="Global Active Power (kilowatts")
# convert to png and copy to working dir.
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
#-----------------------Create Plot 2.png ---------------------------------------------
par(mfrow = c(1,1),mar= c(4,4,2,1) )

# Combine Date and Time column value into a Date/Time value.  Tried mutate but mutate does not support POSIX format.
#selData1 <- mutate(selData,DateTime = strptime(paste(selData$Date,selData$Time),"%m/%d/%y %H:%M:%S"))
# Therefore create a new vector called DateTime.
DateTime <- strptime(paste(selData$Date,selData$Time),"%d/%m/%Y %H:%M:%S")
names(DateTime) <- "DateTime"
selData <- cbind(selData,DateTime)
plot(selData$Global_active_power ~ selData$DateTime, type = "l",xlab = "",ylab = "Global Active Power (kilowatts)")
dev.copy(png, file="plot2.png", width=480, height=480)
#-----------------------Create Plot 3.png ---------------------------------------------

plot(selData$Sub_metering_1 ~ selData$DateTime, type = "l", col="black",xlab="",ylab="Energy sub metering")
lines(selData$Sub_metering_2 ~ selData$DateTime ,col="red")
lines(selData$Sub_metering_3 ~ selData$DateTime ,col="blue")
legend("topright",col=c("black","red","blue"),c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1)
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
#-----------------------Create Plot 4.png ---------------------------------------------
par(mfrow=c(2,2))
plot(selData$Global_active_power ~ selData$DateTime,type="l",xlab="",ylab="Global Active_power")
plot(selData$Voltage ~ selData$DateTime,type="l",xlab="",ylab="Voltage")
plot(selData$Sub_metering_1 ~ selData$DateTime,type="l",xlab="",ylab="Energy sub metering")
lines(selData$Sub_metering_2 ~ selData$DateTime, col="red")
lines(selData$Sub_metering_3 ~ selData$DateTime, col="blue")
legend("topright",col=c("black","red","blue"),c("sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,box.lwd = 0)
plot(selData$Global_reactive_power ~ selData$DateTime,type="n",xlab="datetime",ylab="Global_reactive_power")
lines(selData$Global_reactive_power ~ selData$DateTime)
dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()
