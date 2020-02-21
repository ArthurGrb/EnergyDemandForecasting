# Set locale to English language
Sys.setlocale("LC_TIME", "C")

# To install packages, only necessary the first time, comment out after!

# install.packages("timeSeries")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("astsa")
# install.packages("lubridate")
# install.packages("forecast")

# TO REMOVE
# rm(list = ls())

# Set libraries
#library(tidyverse)
library(timeSeries)
library(dplyr)
library(ggplot2)
library(astsa)
library(lubridate)
library(forecast)

#https://dataminer2.pjm.com/feed/hrl_load_metered 
# DANS PJM Selectionner data du 1er Janvier ? 01:00 au 31 D?cembre ? 00:00 pour avoir des blocs de 24heures
# Ici donn?es du 1er Janvier 2006 ? 01:00 au 31 D?cembre 2018 ? 00:00
# SERC SOUTH DOM DOM
#https://www.dominionenergy.com/

par(mfrow = c(1,1))


# input path to data on your computer, replace back slash with forward slash
path <- "C:~/Documents/School/HEC/Winter19/6-638/Projet/"

# reading data
original_data <- read.table(paste(path, "hrl_load_metered.csv", sep = ""), header=TRUE, sep=",")

# Quick clean up
# we want to keep only datetime_beginning_utc and mw
mydata <- original_data[,c(1,7)]

# rename date
names(mydata)[1]<-"date_time"

# Quick check of the data
str(mydata)
summary(mydata)
head(mydata)

# format: ("2012-01-01 00:00:00"), ann?e mois jour
mydata[,1] <- format(as.POSIXct(as.character(mydata[,1]), format = '%m/%d/%Y %I:%M:%S %p', tz = "UTC"), "%Y-%m-%d %H:%M:%S")

# Finding duplicates
dupl_dates <- data.frame(c(mydata[duplicated(mydata[,1], fromLast = FALSE),], mydata[duplicated(mydata[,1], fromLast = TRUE),]))

# Sum to get daily demand
mydata$date <- as.Date(mydata$date_time)
mydata <- aggregate(mydata$mw ~ mydata$date, FUN=sum)
colnames(mydata) <- c("date", "mw")

# Removing February 29th
# mydata <- mydata[which(month(mydata[,1]) != 2 | day(mydata[,1]) != 29),]
# Removing 2005 and 2019 (not complete data)
mydata <- mydata[which(year(mydata[,1]) != 2005 & year(mydata[,1]) != 2019),]

# Variable for days of the week
mydata$dayoftheweek <- weekdays(mydata[,1])

# Dummy variable for weekends
mydata$weekend <- isWeekend(mydata[,1])

# Variable for holidays
holidays <- read.csv(paste(path, "holidays.csv", sep = ""), header=TRUE)
holidays[,1] <- format(as.POSIXct(as.character(holidays[,1]), format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")
mydata[,1] <- format(as.POSIXct(as.character(mydata[,1]), format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")
colnames(holidays) <- c("date", "holidayName", "holidayType")
mydata <- merge(x = mydata, y = holidays, by = c("date"), all.x = TRUE)

# Dummy variable for day off
mydata$dayoff <- isWeekend(mydata[,1]) | !is.na(mydata[,5])

# creation of the timeseries
data_bis <- timeSeries(mydata[,2], mydata[,1], format = "%Y-%m-%d")

# Daily Demand plot from 2006-2018
plot(data_bis,xlab = "Date", ylab = "Demande journali?re d'?lectricit? (en MW/h)", main="Figure 1: Demande journali?re d'?lectricit? (en MW/h) de 2006-01-01 ? 2018-12-31", at="pretty")

# Daily Demand plot from 2015-2016
mydata20152016 <- mydata[which(mydata[,1] > as.POSIXct('2014-12-31', tz ="UTC") & mydata[,1] < as.POSIXct('2017-01-01', tz ="UTC")),]
plot(timeSeries(mydata20152016[,2], mydata20152016[,1], format = "%Y-%m-%d"),xlab = "Date", ylab = "Demande journali?re d'?lectricit? (en MW/h)", main="Figure 2: Demande journali?re d'?lectricit? (en MW/h) de 2015-01-01 ? 2016-12-31", at="pretty")
abline(v = as.POSIXct('2015-02-01'), col = "blue")
abline(v = as.POSIXct('2015-08-01'), col = "red")
abline(v = as.POSIXct('2016-02-01'), col = "blue")
abline(v = as.POSIXct('2016-08-01'), col = "red")

# Boxplots days of the week/weekends
boxplot(mydata$mw ~ mydata$dayoftheweek,xlab = "Jour de la semaine", ylab = "Demande journali?re d'?lectricit? (en MW/h)", main = "Figure 3: Demande journali?re d'?lectricit? (en MW/h) par jour de la semaine")

# Boxplots for days off
boxplot(mydata$mw ~ mydata$dayoff, names = c("Jour de travail", "Fin de semaine ou cong? f?ri?"), xlab = "Jour de la semaine", ylab = "Demande journali?re d'?lectricit? (en MW/h)", main = "Figure 4: Demande journali?re d'?lectricit? (en MW/h) par type de jour")

# Data with 400,000+ MW/day
outliers = mydata[which(mydata[,2] > 400000), c(1:2)]

# Reading temperature data
weather1 <- read.csv("~/Documents/School/HEC/Winter19/6-638/Projet/WeatherData.csv")
weather[,3] <- format(as.POSIXct(as.character(weather[,3]), format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")

# Aggregating data by average of 4 stations
dailyweather <- aggregate(weather[,4] ~ weather[,3], FUN=mean)

dailyweather2 = data.frame(weather$TAVG, weather$DATE)

colnames(dailyweather) <- c("date", "temp")

# Removing Feb 29th
# dailyweather <- dailyweather[which(month(dailyweather[,1]) != 2 | day(dailyweather[,1]) != 29),]

# Merging with mydata
mydata <- merge(x = mydata, y = dailyweather, by = c("date"), all.x = TRUE)

Tt <- timeSeries(mydata[,8], mydata[,1], format = "%Y-%m-%d")
plot(series(Tt),series(data_bis), main = "Figure 5: Demande journali?re d'?lectricit? (en MW/h) par temp?rature (en celsius)",
          ylab="Demande journali?re d'?lectricit? (en MW/h)",
          xlab="Temp?rature (en celsius)", pch = 21, cex = 2, cex.main = 3, cex.lab = 2, cex.axis = 1.5)
abline(v = 15, col = "blue")

# Reference Temperature
Tref <- 15

mydata[,8] - Tref

# CDD
mydata$CDD <- mydata[,8] - Tref
mydata$CDD <- ifelse(mydata$CDD < 0, 0,mydata$CDD)
CDDt <- timeSeries(mydata$CDD, mydata[,1], format = "%Y-%m-%d")
plot(series(CDDt),series(data_bis), main = "Figure 6: Demande journali?re d'?lectricit? (en MW/h) par CDD",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="CDD", pch = 21, cex = 2, cex.main = 3, cex.lab = 2, cex.axis = 1.5)

# HDD
mydata$HDD <- Tref - mydata[,8]
mydata$HDD <- ifelse(mydata$HDD < 0, 0,mydata$HDD)
HDDt <- timeSeries(mydata$HDD, mydata[,1], format = "%Y-%m-%d")
plot(series(HDDt),series(data_bis), main = "Figure 7: Demande journali?re d'?lectricit? (en MW/h) par HDD",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="HDD", pch = 21, cex = 2, cex.main = 3, cex.lab = 2, cex.axis = 1.5)

# Graphs for lags
par(mfrow = c(2,2))

plot(series(lag(HDDt,1)),series(data_bis), main = "Figure 8: Demande journali?re d'?lectricit? (en MW/h) par lag(HDD,1)",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="lag(HDD,1)", pch = 21, cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1.5)
plot(series(lag(HDDt,2)),series(data_bis), main = "Figure 9: Demande journali?re d'?lectricit? (en MW/h) par lag(HDD,2)",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="lag(HDD,2)", pch = 21, cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1.5)
plot(series(lag(CDDt,1)),series(data_bis), main = "Figure 10: Demande journali?re d'?lectricit? (en MW/h) par lag(CDD,1)",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="lag(CDD,1)", pch = 21, cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1.5)
plot(series(lag(CDDt,2)),series(data_bis), main = "Figure 11: Demande journali?re d'?lectricit? (en MW/h) par lag(CDD,2)",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="lag(CDD,2)", pch = 21, cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1.5)

# Boxplot for month
par(mfrow = c(1,1))
boxplot(mydata$mw ~ month(mydata$date, label = TRUE), xlab = "Mois", ylab = "Demande journali?re d'?lectricit? (en MW/h)", main = "Figure 12: Demande journali?re d'?lectricit? (en MW/h) par mois", pch = 21, cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1.5)

# Training Set
trainingset <- mydata[which(mydata[,1] < '2017-01-01'),c(1:2)]
TStrainingset <- timeSeries(trainingset[,2], trainingset[,1], format = "%Y-%m-%d")

# Validation Set
validationset <- mydata[which(mydata[,1] < '2018-01-01' & mydata[,1] > '2016-12-31'),c(1:2)]
TSvalidationset <- timeSeries(validationset[,2], validationset[,1], format = "%Y-%m-%d")

# Test Set
testset <- mydata[which(mydata[,1] < '2019-01-01' & mydata[,1] > '2017-12-31'),c(1:2)]
TStestset <- timeSeries(testset[,2], testset[,1], format = "%Y-%m-%d")

########################################################################
# Training Set
########################################################################

# (1) No-change model forecast
TS_nochange = trainingset[[2]][-length(trainingset[[2]])]
TS_observed_nochange <- trainingset[[2]][-1]

TS_bias_nochange <- mean(TS_nochange - TS_observed_nochange)
TS_pbias_nochange <- mean((TS_nochange - TS_observed_nochange) / TS_observed_nochange) * 100
TS_mape_nochange <- mean(abs((TS_nochange - TS_observed_nochange) / TS_observed_nochange)) * 100

# (2) No-change yearly seasonal model forecast
TS_nochangeyearly <- trainingset[[2]][1:(length(trainingset[[2]])-364)]
TS_observed_nochangeyearly <- trainingset[[2]][365:(length(trainingset[[2]]))]

TS_bias_nochangeyearly <- mean(TS_nochangeyearly-TS_observed_nochangeyearly)
TS_pbias_nochangeyearly <- mean((TS_nochangeyearly-TS_observed_nochangeyearly)/TS_observed_nochangeyearly)*100
TS_mape_nochangeyearly <- mean(abs((TS_nochangeyearly-TS_observed_nochangeyearly)/TS_observed_nochangeyearly))*100

# Computing (3) rolling three-day window
TS_rolling3 <- rollMean(TStrainingset,3,align="right",na.pad=T)[-(1:2)]
TS_rolling3 <- TS_rolling3[-length(TS_rolling3)]
TS_observed_rolling3 <- trainingset[[2]][-(1:3)]

TS_bias_rolling3 <- mean(TS_rolling3-TS_observed_rolling3)
TS_pbias_rolling3 <- mean((TS_rolling3 - TS_observed_rolling3) / TS_observed_rolling3) * 100
TS_mape_rolling3 <- mean(abs((TS_rolling3 - TS_observed_rolling3) / TS_observed_rolling3)) * 100


########################################################################
# Validation Set
########################################################################

# (1) No-change model forecast
VS_nochange = validationset[[2]][-length(validationset[[2]])]
VS_observed_nochange <- validationset[[2]][-1]

VS_bias_nochange <- mean(VS_nochange - VS_observed_nochange)
VS_pbias_nochange <- mean((VS_nochange - VS_observed_nochange) / VS_observed_nochange) * 100
VS_mape_nochange <- mean(abs((VS_nochange - VS_observed_nochange) / VS_observed_nochange)) * 100

# (2) No-change yearly seasonal model forecast
VS_nochangeyearly <- trainingset[[2]][-(1:(length(trainingset[[2]])-364))]
VS_observed_nochangeyearly <- validationset[[2]][-length(validationset[[2]])]

VS_bias_nochangeyearly <- mean(VS_nochangeyearly-VS_observed_nochangeyearly)
VS_pbias_nochangeyearly <- mean((VS_nochangeyearly-VS_observed_nochangeyearly)/VS_observed_nochangeyearly)*100
VS_mape_nochangeyearly <- mean(abs((VS_nochangeyearly-VS_observed_nochangeyearly)/VS_observed_nochangeyearly))*100

# Computing (3) rolling three-day window
VS_rolling3 <- rollMean(TSvalidationset,3,align="right",na.pad=T)[-(1:2)]
VS_rolling3 <- VS_rolling3[-length(VS_rolling3)]
VS_observed_rolling3 <- validationset[[2]][-(1:3)]

VS_bias_rolling3 <- mean(VS_rolling3-VS_observed_rolling3)
VS_pbias_rolling3 <- mean((VS_rolling3 - VS_observed_rolling3) / VS_observed_rolling3) * 100
VS_mape_rolling3 <- mean(abs((VS_rolling3 - VS_observed_rolling3) / VS_observed_rolling3)) * 100



