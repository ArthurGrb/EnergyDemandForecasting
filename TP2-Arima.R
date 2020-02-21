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
path <- "~/Documents/School/HEC/Winter19/6-638/Projet/Equipe3_Gadbois_Franklin_Gerbe/"

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

library(lubridate)

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

dayoftheweek_test <- weekdays(mydata[,1])
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
weather <- read.csv(paste(path, "WeatherData.csv", sep = ""), header=TRUE)
weather[,3] <- format(as.POSIXct(as.character(weather[,3]), format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")

# Aggregating data by average of 4 stations
dailyweather <- aggregate(weather[,4] ~ weather[,3], FUN=mean)
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
mydata$CDD <- as.numeric(mydata$CDD)
CDDt <- timeSeries(mydata$CDD, mydata[,1], format = "%Y-%m-%d")
plot(series(CDDt),series(data_bis), main = "Figure 6: Demande journali?re d'?lectricit? (en MW/h) par CDD",
     ylab="Demande journali?re d'?lectricit? (en MW/h)",
     xlab="CDD", pch = 21, cex = 2, cex.main = 3, cex.lab = 2, cex.axis = 1.5)

# HDD
mydata$HDD <- Tref - mydata[,8]
mydata$HDD <- ifelse(mydata$HDD < 0, 0,mydata$HDD)
mydata$HDD <- as.numeric(mydata$HDD)
HDDt <- timeSeries(as.numeric(c(mydata$HDD)), mydata[,1], format = "%Y-%m-%d")
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
library(forecast)
#library(fracdiff)

in.sample.data <- series(TStrainingset)
in.sample <- msts(in.sample.data,seasonal.periods=c(7,365.25))
out.sample.data <- series(TSvalidationset)
out.sample <- msts(out.sample.data,seasonal.periods=c(7,365.25))

#Creating the dummy variables, friday as base
DMon <- ifelse(factor(dayoftheweek_test)=="Monday",1,0)
DTue <- ifelse(factor(dayoftheweek_test)=="Tuesday",1,0)
DWed <- ifelse(factor(dayoftheweek_test)=="Wednesday",1,0)
DThu <- ifelse(factor(dayoftheweek_test)=="Thursday",1,0)
DSat <- ifelse(factor(dayoftheweek_test)=="Saturday",1,0)
DSun <- ifelse(factor(dayoftheweek_test)=="Sunday",1,0)


fit <- auto.arima(Tt,xreg=cbind(
  mydata$HDD,
  mydata$CDD,
  lag(mydata$HDD,1),lag(mydata$HDD,2),
  lag(mydata$CDD,1),lag(mydata$CDD,2),
  DMon,DTue,DWed,DThu,DSat,DSun)) 

print(fit)

n <- length(out.sample)

newxreg1 <- cbind(mydata$HDD[n],mydata$CDD[n],
                  mydata$HDD[n],mydata$HDD[n-1],
                  mydata$CDD[n],mydata$CDD[n-1],0,1,0,0,0,0)
##Not working
pred.fit <- forecast(fit, h = length(out.sample), xreg = newxreg1)

#Even when removing first 2 residuals, error message remains: missing values in object
acf(residuals(fit)[-(1:2)],
    main="With proper error structure (using auto.arima)")

#Using arima with (3,0,0)
#I removed CDD because of error message system is computationally
#singular. meaning that one regressor should be removed
fit.arima <- arima(Tt, xreg=cbind( 
  mydata$HDD,
  lag(mydata$HDD,1),lag(mydata$HDD,2),
  DMon,DTue,DWed,DThu,DSat,DSun), order=c(3,0,0))
print(fit.arima)

fit.arima.res <- c(residuals(fit.arima)[-(1:2)])
acf(fit.arima.res,
    main="With proper error structure (using order=c(3,0,0))", na.action = na.action)

#results in a lower AIC: 16460.78
print(fit.arima)

#Forecasting using fit.arima: we have to supply newxreg matrix
#First day of 2019 is Tuesday so 0,1,0,0,0,0,
n <- length(out.sample)
#xreg newxreg have different numbers of columns because of CDD so I removed CDD
newxreg2 <- cbind(mydata$HDD[n],
                  mydata$HDD[n],mydata$HDD[n-1],0,1,0,0,0,0)

#pred1 <- predict(fit.arima,n.ahead=1,newxreg=newxreg1)

#One day ahead
pred1 <- predict(fit.arima,n.ahead=1,newxreg=newxreg2)
print(pred1)
accuracy(pred1)



#fc <- forecast(fit, xreg=cbind(
#  validationset$HDD,
#  validationset$CDD,
 # lag(validationset$HDD,1),lag(validationset$HDD,2),
 # lag(validationset$CDD,1),lag(validationset$CDD,2),
 # DMon,DTue,DWed,DThu,DSat,DSun))
#print(accuracy(fit))
