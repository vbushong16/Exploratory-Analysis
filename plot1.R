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

#Plot 1
png(filename = "plot1.png",width = 480, height = 480)
hist(data[,"Global_active_power"],breaks = 20, xlab = " Global Active Power (kilowatts)", main = "Global Active Power", col ="red")
dev.off()