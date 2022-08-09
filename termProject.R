getwd()
importedInfo <- read.table(file = "TermProjectData.txt", header = TRUE, sep = ",")

#Part 1

#Thursdays at 5pm (17:00)-8pm (20:00)
importedInfo$Date_time <- as.POSIXct(paste(importedInfo$Date, importedInfo$Time), format = "%d/%m/%Y %H:%M:%S")

#install.packages("lubridate")
library(lubridate)
thursday <- subset(importedInfo, wday(importedInfo$Date_time, week_start = 1) == 4)
thursdayWTime <- subset(thursday, hour(thursday$Date_time) >= 17 & hour(thursday$Date_time) < 20)
toBeRemoved <- c("Date", "Time", "Date_time")
filteredValues <- thursdayWTime[, !(names(thursdayWTime) %in% toBeRemoved)]
scaledValues <- as.data.frame(scale(filteredValues))

#PCA
library(stats)
pca_res <- prcomp(scaledValues)
#Extracting loading scores from PCA results
print(pca_res)
loadingScores <- abs(pca_res$rotation[,1])
#Sorting loading scores
sortedLoadingScores <- loadingScores[order(loadingScores, decreasing=TRUE)]
print(sortedLoadingScores)
#Based on loading scores we will use Global_intensity, Global_active_power, and Voltage, since those are the
#3 highest loading scores (arbitrary, just seems like it could be enough to retain most of the information)

#install.packages("ggfortify")
#Plotting the PCA results
library(ggfortify)
autoplot(pca_res)

#Part 2

processedData <- subset(scaledValues, select=c("Global_intensity", "Global_active_power", "Voltage"))
trainData <- processedData[1:20160,]
testData <- processedData[20161:27720,]

nstatesValues <- c(4,6,8,10,12,14,16,18,20,22,24)
logLiks <- rep(0, times = length(nstatesValues))
testLogLiks <- rep(0, times = length(nstatesValues))
BICs <- rep(0, times = length(nstatesValues))

#install.packages("depmixS4")
library(depmixS4)

for (val in nstatesValues) {
  ntimes112 <- rep(180, times = 112)
  model <- depmix(response = list(trainData$Global_intensity ~ 1, trainData$Global_active_power ~ 1, trainData$Voltage ~ 1), data = trainData, nstates = val, ntimes = ntimes112, family = list(gaussian(), gaussian(), gaussian()))
  fitModel <- fit(model)
  #extract log-likelihood and BIC
  logLiks[(val - 2)/2] <- logLik(fitModel)
  BICs[(val - 2)/2] <- BIC(fitModel)
  
  summary(fitModel)
  
  #Testing on training data
  ntimes42 <- rep(180, times = 42)
  testModel <- depmix(response = list(testData$Global_intensity ~ 1, testData$Global_active_power ~ 1, testData$Voltage ~ 1), data = testData, nstates = val, ntimes = ntimes42, family = list(gaussian(), gaussian(), gaussian()))
  testModel <- setpars(testModel,getpars(fitModel))
  fbTest <- forwardbackward(testModel)
  testLogLiks[(val - 2)/2] <- fbTest$logLike
  
  #chosen model
  if (val == 12) {
    selectedModelParams <- getpars(fitModel)
  }
}
print(logLiks)
print(testLogLiks)
print(BICs)
scaledLogLiks <- scale(logLiks)
scaledBICs <- scale(BICs)
scaledTestLogLiks <- scale(testLogLiks)
print(scaledLogLiks)
print(scaledTestLogLiks)
print(scaledBICs)

plot(nstatesValues, scaledLogLiks, type="b", col="blue", lwd=2, xlab="nstates", ylab="Log-Likelihood, Test Log-Likelihood, and BIC")
lines(nstatesValues, scaledBICs, type="b", col="red", lwd=2)
lines(nstatesValues, scaledTestLogLiks, type="b", col="green", lwd=2)
legend(3,1,c("Log-Likelihood","BIC", "Test Log-Likelihoods"), lwd=c(2,2), col=c("blue","red","green"), y.intersp=1, xjust=-1, yjust = 3.5)

#Part 3

#Importing data with anomalies
importedInfo1 <- read.table(file = "DataWithAnomalies1.txt", header = TRUE, sep = ",")
importedInfo2 <- read.table(file = "DataWithAnomalies2.txt", header = TRUE, sep = ",")
importedInfo3 <- read.table(file = "DataWithAnomalies3.txt", header = TRUE, sep = ",")

#Thursdays at 5pm (17:00)-8pm (20:00)
importedInfo1$Date_time <- as.POSIXct(paste(importedInfo1$Date, importedInfo1$Time), format = "%d/%m/%Y %H:%M:%S")
importedInfo2$Date_time <- as.POSIXct(paste(importedInfo2$Date, importedInfo2$Time), format = "%d/%m/%Y %H:%M:%S")
importedInfo3$Date_time <- as.POSIXct(paste(importedInfo3$Date, importedInfo3$Time), format = "%d/%m/%Y %H:%M:%S")
thursday1 <- subset(importedInfo1, wday(importedInfo1$Date_time, week_start = 1) ==4)
thursday2 <- subset(importedInfo2, wday(importedInfo2$Date_time, week_start = 1) ==4)
thursday3 <- subset(importedInfo3, wday(importedInfo3$Date_time, week_start = 1) ==4)
thursdayWTime1 <- subset(thursday1, hour(thursday1$Date_time) >= 17 & hour(thursday1$Date_time) < 20)
thursdayWTime2 <- subset(thursday2, hour(thursday2$Date_time) >= 17 & hour(thursday2$Date_time) < 20)
thursdayWTime3 <- subset(thursday3, hour(thursday3$Date_time) >= 17 & hour(thursday3$Date_time) < 20)
toBeRemoved <- c("Date", "Time", "Date_time")
filteredValues1 <- as.data.frame(thursdayWTime1[, !(names(thursdayWTime1) %in% toBeRemoved)])
filteredValues2 <- as.data.frame(thursdayWTime2[, !(names(thursdayWTime2) %in% toBeRemoved)])
filteredValues3 <- as.data.frame(thursdayWTime3[, !(names(thursdayWTime3) %in% toBeRemoved)])
scaledValues1 <- as.data.frame(scale(filteredValues1))
scaledValues2 <- as.data.frame(scale(filteredValues2))
scaledValues3 <- as.data.frame(scale(filteredValues3))
processedData1 <- subset(scaledValues1, select=c("Global_intensity", "Global_active_power", "Voltage"))
processedData2 <- subset(scaledValues2, select=c("Global_intensity", "Global_active_power", "Voltage"))
processedData3 <- subset(scaledValues3, select=c("Global_intensity", "Global_active_power", "Voltage"))

ntimes52 <- rep(180, times = 52)
testModel1 <- depmix(response = list(processedData1$Global_intensity ~ 1, processedData1$Global_active_power ~ 1, processedData1$Voltage ~ 1), data = processedData1, nstates = 12, ntimes = ntimes52, family = list(gaussian(), gaussian(), gaussian()))
testModel1 <- setpars(testModel1,selectedModelParams)
anomalyDataTest1 <- forwardbackward(testModel1)

testModel2 <- depmix(response = list(processedData2$Global_intensity ~ 1, processedData2$Global_active_power ~ 1, processedData2$Voltage ~ 1), data = processedData2, nstates = 12, ntimes = ntimes52, family = list(gaussian(), gaussian(), gaussian()))
testModel2 <- setpars(testModel2,selectedModelParams)
anomalyDataTest2 <- forwardbackward(testModel2)

testModel3 <- depmix(response = list(processedData3$Global_intensity ~ 1, processedData3$Global_active_power ~ 1, processedData3$Voltage ~ 1), data = processedData3, nstates = 12, ntimes = ntimes52, family = list(gaussian(), gaussian(), gaussian()))
testModel3 <- setpars(testModel3,selectedModelParams)
anomalyDataTest3 <- forwardbackward(testModel3)


trainingLogLike12 <- logLiks[5]
print(trainingLogLike12)
print(anomalyDataTest1$logLike)
print(anomalyDataTest2$logLike)
print(anomalyDataTest3$logLike)
