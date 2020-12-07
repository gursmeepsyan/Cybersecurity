library(dplyr)
library(ggplot2)
library(ggbiplot)
library(pracma)
library(corrplot)
library(data.table)
library(tidyverse)
library(depmixS4)
library(stats)

getwd();
setwd("/Users/sehajvir/Desktop/CMPT318")
# Clean the data: remove the NA entries for the data 
initial_data <- read.csv("TermProjectData.txt")
clean_data <- na.omit(initial_data)

dataTime <- as.ITime(format(clean_data$Time, format = "%H:%M:%S"))
clean_data$STime <- as.integer(clean_data$Time) 
startTime <- as.ITime("09:00")
endTime <- as.ITime("13:00")

clean_data$day <- ifelse((dataTime >= startTime & dataTime <= endTime), "window", "other")

# Dataframe for Wednesday for 09:00AM - 13:00PM
filteredData <- filter(clean_data,
                       daysWeek == "3",
                       day == "window")


# Seting up the training data for Global Intensity
trainIntensityData <- filter(filteredData, year < "9")
trainIntensityData <- trainIntensityData[c(1, 6)]
testIntensityData <- filter(filteredData, year == "9")
testIntensityData <- testIntensityData[c(1, 6)]

### Instantiate Groups for Training and Testing
trainGroup <- group_by(trainIntensityData, Date)
trainGroupCount <- summarise(trainGroup, count = n())
testGroup <- group_by(testIntensityData, Date)
testGroupCount <- summarise(testGroup, count = n())

set.seed(298224)

# Min, Max and the Total number of states
minState <- 2
maxState <- 20
numStates <- 2:20

# Container for BIC
BIC.vector = c("States", "BICs", "logLik")
BIC.models = array(0, dim = c(maxState, length(BIC.vector)))
colnames(BIC.models) = BIC.vector

for (i in numStates)
{
  model = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = i,
                 family = gaussian(), data = trainIntensityData, 
                 ntimes = trainGroupCount$count)
  
  fitModel <- fit(model)
  
  BIC.models[i, "States"] <- i
  BIC.models[i, "BICs"] <- BIC(fitModel)
  BIC.models[i, "logLik"] <- logLik(fitModel)
}
BIC.models

#Ploting BIC wrt Loglik
par(mar = c(5, 5, 3, 5))
plot(BIC.models[1:20, 2], type ="l", ylab = "BIC",
     main = "BIC v/s LogLik",
     xlab = "Number Of States", col = "green")

par(new = TRUE)
plot(BIC.models[1:20,3], type = "l", xaxt = "n",
     yaxt = "n", ylab = "", 
     xlab = "", col = "red", lty = 2)

axis(side = 4)
axis(side = 1, at = seq(0, 20, by = 5))

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BIC", "LogLik"), 
       col = c("green", "red"), lty = c(1, 2))

# Read in Anomaly Data
anomalyData1 = read.table("Data1(WithAnomalies).txt", sep = ',', header = TRUE)
anomalyData2 = read.table("Data2(WithAnomalies).txt", sep = ',', header = TRUE)
anomalyData3 = read.table("Data3(WithAnomalies).txt", sep = ',', header = TRUE)

#Create dataframes for anomaly1, anomaly2, anomaly3
anomaly1IntensityData <- setAnomalyData(anomalyData1)
anomaly2IntensityData <- setAnomalyData(anomalyData2)
anomaly3IntensityData <- setAnomalyData(anomalyData3)


# Create group for anomaly data set 1
a1Group <- group_by(anomaly1IntensityData, Date)
a1GroupCount <- summarise(a1Group, count = n())

set.seed(298224)

# Training for ideal
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, 
                             nstates = 14, family = gaussian(), data = trainIntensityData,
                             ntimes = trainGroupCount$count)
trainFitModel <- fit(trainIntensityModel)
trainIntensityObject <- forwardbackward(trainFitModel)

# Test for ideal state with anomaly 1
a1IntensityModel = depmix(response = anomaly1IntensityData$Global_intensity ~ 1, 
                          nstates = 14, family = gaussian(), data = anomaly1IntensityData,
                          ntimes = a1GroupCount$count)

a1Intensity <- setpars(a1IntensityModel, getpars(trainFitModel))

a1IntensityObject <- forwardbackward(a1Intensity)

(trainIntensityObject$logLike)
(a1IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly1IntensityData))

# Create group for anomaly data set 2
a2Group <- group_by(anomaly2IntensityData, Date)
a2GroupCount <- summarise(a2Group, count = n())

set.seed(298224)

# Training model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1,
                             nstates = 14, family = gaussian(), 
                             data = trainIntensityData, ntimes = trainGroupCount$count)

trainFitModel <- fit(trainIntensityModel)

trainIntensityObject <- forwardbackward(trainFitModel)

# Test model with anomaly data 1 for ideal state
a2IntensityModel = depmix(response = anomaly2IntensityData$Global_intensity ~ 1,
                          nstates = 14, family = gaussian(), data = anomaly2IntensityData, 
                          ntimes = a2GroupCount$count)

a2Intensity <- setpars(a2IntensityModel, getpars(trainFitModel))
a2IntensityObject <- forwardbackward(a2Intensity)

(trainIntensityObject$logLike)
(a2IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly2IntensityData))

# Create group for anomaly data set 2
a3Group <- group_by(anomaly3IntensityData, Date)
a3GroupCount <- summarise(a3Group, count = n())
set.seed(298224)

# Training model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = 14, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)
trainFitModel <- fit(trainIntensityModel)

# Use forwardbackward algorithm
trainIntensityObject <- forwardbackward(trainFitModel)

a3IntensityModel = depmix(response = anomaly3IntensityData$Global_intensity ~ 1, nstates = 14, family = gaussian(), data = anomaly3IntensityData, ntimes = a3GroupCount$count)

a3Intensity <- setpars(a3IntensityModel, getpars(trainFitModel))

a3IntensityObject <- forwardbackward(a3Intensity)

(trainIntensityObject$logLike)
(a3IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly3IntensityData))

