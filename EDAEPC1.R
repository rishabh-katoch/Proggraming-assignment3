EDA3CP1<-read.table(choose.files(),header = TRUE, sep=";")

#checking the datatype and how the data looks like 
str(EDA3CP1)

# changing the datatype of date 

EDA3CP1$Date <- as.Date(EDA3CP1$Date, "%d/%m/%Y")
EDA3CP1$Global_active_power <- as.numeric(EDA3CP1$Global_active_power)

# filtering date

EDA3CP1 <- subset(EDA3CP1,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#removing incomplete observations
EDA3CP1 <- EDA3CP1[complete.cases(EDA3CP1),]

## Combine Date and Time column
dateTime <- paste(EDA3CP1$Date, EDA3CP1$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
EDA3CP1 <- EDA3CP1[ ,!(names(EDA3CP1) %in% c("Date","Time"))]

## Add DateTime column
EDA3CP1 <- cbind(dateTime, EDA3CP1)

## Format dateTime Column
EDA3CP1$dateTime <- as.POSIXct(dateTime)


## Create the histogram
hist(EDA3CP1$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
dev.copy(png,"plot1.png", width=480, height=480)


## Create Plot 2
plot(EDA3CP1$Global_active_power~EDA3CP1$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot2.png", width=480, height=480)

## Create Plot 3
with(EDA3CP1, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png,"plot3.png", width=480, height=480)

## Create Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(EDA3CP1, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

dev.copy(png,"plot4.png", width=480, height=480)