## plot2.R
## created by Li Lu (llu5@yahoo.com)
## Date 2/20/2016


## first read data of selected data 
## from the dates 2007-02-01 and 2007-02-02
install.packages("dplyr")
install.packages("downloader")

library(dplyr)
## create folder data and download file by using downloader package
if(!file.exists("./data")){dir.create("./data")}
fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
require(downloader)

download(fileUrl, "data/household_power_consumption.zip", mode = "wb") 

## unzip the consumption.zip file to create a folder named "household_power_consumption"
unzip(zipfile="./data/household_power_consumption.zip",exdir="./data")


plot1Data <-read.table("./data/household_power_consumption.txt",  header = TRUE, sep=";", stringsAsFactors = FALSE, na.strings = "?") ## replace "?" as NA
plot1Data<-plot1Data[complete.cases(plot1Data),]  ## remove rows with NA values.
data_tbl <- tbl_df(plot1Data)   ## create a data frame tbl to wrap a local data frame.
filtered_data <- filter(data_tbl, Date == "2/2/2007" | Date == "1/2/2007") ## return rows with matched dates.


filtered_data_m <- mutate(filtered_data, Date_Time = paste(Date, Time, sep = " "))  ## add new type character variable Date_Time by concatenating date and time fields
filtered_data_m$Date_Time <- strptime(filtered_data_m$Date_Time, "%d/%m/%Y %H:%M:%S") ## convert character to date-time "POSIXlt" "POSIXt"
filtered_data_m[, 3:9] <- lapply(filtered_data_m[, 3:9], as.numeric)  ## change columns 3 to 9 from character to number
filtered_data_m <- filtered_data_m[, c(10, 3:9)] ## keep column 10, Date_Time, and columns 3:9, columns of original Date and Time are dropped

filtered_data_m <- rename(filtered_data_m, kitchen = Sub_metering_1,
                          laundry_room= Sub_metering_2,  heater_air_conditioning= Sub_metering_3)  ## rename three sub metering columns in a more meaningful description
png("plot3.png", width = 480, height=480, res=78)

kitchen <- filtered_data_m$kitchen
laundry_room <- filtered_data_m$laundry_room
heater_air_conditioning <- filtered_data_m$heater_air_conditioning

energy_sub_metering =c(kitchen,laundry_room,heater_air_conditioning)

plot(filtered_data_m$Date_Time, kitchen, type = "l", ylab = "Energy sub metering", xlab = "")
points(filtered_data_m$Date_Time, laundry_room, type = "l", col = "red")
points(filtered_data_m$Date_Time, heater_air_conditioning, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()