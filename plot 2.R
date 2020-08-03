
## Create Plot 2
plot(EDA3CP1$Global_active_power~EDA3CP1$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot2.png", width=480, height=480)
