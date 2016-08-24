### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\Exploratory-Analysis"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))

#zip file download to director
url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(url, destfile = "household_power_consumption.zip")

data = read.table(unz("household_power_consumption.zip","household_power_consumption.txt"),sep=';',header = TRUE,stringsAsFactors = FALSE)

str(data)
head(data)

data[,"Date"] = as.Date(strptime(data[,"Date"],format = "%d/%m/%Y"))

data = data[data$Date >= "2007-02-01" & data$Date <= "2007-02-02",]

data[,"Global_active_power"] <- as.numeric(data[,"Global_active_power"])
data[,"Global_reactive_power"] <- as.numeric(data[,"Global_reactive_power"])
data[,"Voltage"] <- as.numeric(data[,"Voltage"])
data[,"Global_intensity"] <- as.numeric(data[,"Global_intensity"])
data[,"Sub_metering_1"] <- as.numeric(data[,"Sub_metering_1"])
data[,"Sub_metering_2"] <- as.numeric(data[,"Sub_metering_2"])


#plot 3 
png(filename = "plot3.png",width = 480, height = 480)
plot(data[,"Sub_metering_1"],type = "l",xaxt='n',xlab = "",ylab= "Energy sub metering")
axis(1, at= c(0,length(data[,1])/2,length(data[,1])), labels= c("Thu","Fri","Sat"))
lines(1:length(data[,1]),data[,"Sub_metering_2"],type="l",col="red")
lines(1:length(data[,1]),data[,"Sub_metering_3"],type="l",col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1), lwd=c(1,1,1),col=c("black","red","blue"))
dev.off()