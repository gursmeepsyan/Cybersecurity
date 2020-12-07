library(data.table)
library(stats)
library(ggplot2)
library(plyr)
library(ggbiplot) 
library(dplyr)


getwd()
setwd("F:/Cmpt318/Term-Project")

dataRaw <- read.table("TermProjectData.txt", sep = ',', header = TRUE)
dataRaw <- na.omit(dataRaw)
dateTime <- paste(dataRaw$Date, dataRaw$Time)
dataRaw$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))
# Get only Wednesdays data
wednesdayMornings   <- dataRaw[(as.POSIXlt(dataRaw$Date, format="%d/%m/%Y")$wday == 3 
                           & hour(as.POSIXlt(dataRaw$Time, format="%H:%M:%S")) >= 9 
                           & hour(as.POSIXlt(dataRaw$Time, format="%H:%M:%S")) < 12),]

get_mean<- function(data){
  data$Date <- as.Date(data$Date, "%Y-%m-%d")
  mean_active_power <- aggregate(data["Global_active_power"], by=data["Date"], mean)
  mean_reactive_power <- aggregate(data["Global_reactive_power"], by=data["Date"], mean)
  mean_voltage <- aggregate(data["Voltage"], by=data["Date"], mean)
  mean_intensity <- aggregate(data["Global_intensity"], by=data["Date"], mean)
  mean_metering_1 <- aggregate(data["Sub_metering_1"], by=data["Date"], mean)
  mean_metering_2 <- aggregate(data["Sub_metering_2"], by=data["Date"], mean)
  mean_metering_3 <- aggregate(data["Sub_metering_3"], by=data["Date"], mean)
  mean_total <- merge(mean_active_power,mean_reactive_power, by="Date")
  mean_total <- merge(mean_total,mean_voltage, by="Date")
  mean_total <- merge(mean_total, mean_intensity, by="Date")
  mean_total <- merge(mean_total, mean_metering_1, by="Date")
  mean_total <- merge(mean_total, mean_metering_2, by="Date")
  mean_total <- merge(mean_total, mean_metering_3, by="Date")
  data[ , which(apply(data, 0, var) != 0)]
  pca<- prcomp(mean_total, center = TRUE,scale. = TRUE)
  return(pca)
}
pca <-get_mean(dataRaw)
summary(pca)

