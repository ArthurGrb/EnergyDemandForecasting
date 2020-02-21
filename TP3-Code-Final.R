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
#rm(list = ls())

# Set libraries
#library(tidyverse)
library(timeSeries)
# library(dplyr)
# library(ggplot2)
# library(astsa)
library(lubridate)
library(forecast)
# library(fracdiff)
# #detach("package:timeSeries")
# detach("package:dplyr")
# detach("package:ggplot2")
# detach("package:lubridate")
# detach("package:fracdiff")
# detach("package:astsa")
# detach("package:forecast")


#https://dataminer2.pjm.com/feed/hrl_load_metered 
# DANS PJM Selectionner data du 1er Janvier ? 01:00 au 31 D?cembre ? 00:00 pour avoir des blocs de 24heures
# Ici donn?es du 1er Janvier 2006 ? 01:00 au 31 D?cembre 2018 ? 00:00
# SERC SOUTH DOM DOM
#https://www.dominionenergy.com/

par(mfrow = c(1,1))


# input path to data on your computer, replace back slash with forward slash
path <- "C:/Users/phili/Downloads/"

# reading data
original_data <- read.table(paste(path, "hrl_load_metered.csv", sep = ""),
                            header=TRUE, sep=",")



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
mydata[,1] <- format(as.POSIXct(as.character(mydata[,1]), 
                                format = '%m/%d/%Y %I:%M:%S %p',
                                tz = "UTC"), "%Y-%m-%d %H:%M:%S")


# Finding duplicates
dupl_dates <- data.frame(c(mydata[duplicated(mydata[,1],
                                             fromLast = FALSE),],
                           mydata[duplicated(mydata[,1], fromLast = TRUE),]))

# Sum to get daily demand
mydata$date <- as.Date(mydata$date_time)
mydata <- aggregate(mydata$mw ~ mydata$date, FUN=sum)
colnames(mydata) <- c("date", "mw")

# Removing February 29th
mydata <- mydata[which(month(mydata[,1]) != 2 | day(mydata[,1]) != 29),]
# Removing 2005 and 2019 (not complete data)
mydata <- mydata[which(year(mydata[,1]) != 2005 & year(mydata[,1]) != 2019),]

dayoftheweek_test <- weekdays(mydata[,1])
# Variable for days of the week
mydata$dayoftheweek <- weekdays(mydata[,1])

# Dummy variable for weekends
mydata$weekend <- isWeekend(mydata[,1])

# Variable for holidays
holidays <- read.csv(paste(path, "holidays.csv", sep = ""), header=TRUE)
holidays[,1] <- format(as.POSIXct(as.character(holidays[,1]),
                                  format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")
mydata[,1] <- format(as.POSIXct(as.character(mydata[,1]),
                                format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")
colnames(holidays) <- c("date", "holidayName", "holidayType")
mydata <- merge(x = mydata, y = holidays, by = c("date"), all.x = TRUE)

# Dummy variable for day off
#mydata$dayoff <- isWeekend(mydata[,1]) | !is.na(mydata[,5])
mydata$dayoff <- !is.na(mydata[,5])

# Reading temperature data
weather <- read.csv(paste(path, "WeatherData.csv", sep = ""), header=TRUE)
weather[,3] <- format(as.POSIXct(as.character(weather[,3]),
                                 format = '%Y-%m-%d', tz = "UTC"), "%Y-%m-%d")

# Aggregating data by average of 4 stations
dailyweather <- aggregate(weather[,4] ~ weather[,3], FUN=mean)
colnames(dailyweather) <- c("date", "temp")

# Removing Feb 29th
dailyweather <- dailyweather[which(month(dailyweather[,1]) != 2 | day(dailyweather[,1]) != 29),]

# Merging with mydata
mydata <- merge(x = mydata, y = dailyweather, by = c("date"), all.x = TRUE)

Tt <- timeSeries(mydata[,8], mydata[,1], format = "%Y-%m-%d")

# Reference Temperature
TrefCDD <- 18
TrefHDD <- 13

# CDD
mydata$CDD <- mydata[,8] - TrefCDD
mydata$CDD <- ifelse(mydata$CDD < 0, 0,mydata$CDD)
mydata$CDD <- as.numeric(mydata$CDD)
CDDt <- timeSeries(mydata$CDD, mydata[,1], format = "%Y-%m-%d")

# HDD
mydata$HDD <- TrefHDD - mydata[,8]
mydata$HDD <- ifelse(mydata$HDD < 0, 0,mydata$HDD)
mydata$HDD <- as.numeric(mydata$HDD)
HDDt <- timeSeries(as.numeric(c(mydata$HDD)), mydata[,1], format = "%Y-%m-%d")

# Day of the week
dow <- mydata[,3]
dow <- ifelse(dow=="Sunday",1,ifelse(dow=="Monday",2,
                                     ifelse(dow=="Tuesday",3,
                                            ifelse(dow=="Wednesday",4,
                                                   ifelse(dow=="Thursday",5,
                                                          ifelse(dow=="Friday",6,7))))))
dow <- timeSeries(dow, mydata[,1], format = "%Y-%m-%d")

# Dayoff
dayoff <- timeSeries(mydata$dayoff, mydata[,1], format = "%Y-%m-%d")

# Samples
in.sample <- mydata[which(mydata[,1] < '2016-01-01' & mydata[,1] > '2008-12-31'),c(1:2)]
in.sample.ts <- timeSeries(in.sample[,2], in.sample[,1], format = "%Y-%m-%d")

out.sample <- mydata[which(mydata[,1] < '2018-01-01' & mydata[,1] > '2015-12-31'),c(1:2)]
out.sample.ts <- timeSeries(out.sample[,2], out.sample[,1], format = "%Y-%m-%d")

Yt <- mydata[which(mydata[,1] < '2018-01-01' & mydata[,1] > '2008-12-31'),c(1:2)]
Yt.ts <- timeSeries(Yt[,2], Yt[,1], format = "%Y-%m-%d")

################################################
# Mod?le 1: Last-day no-change
################################################

no.change <- cbind(out.sample[2:length(out.sample[,2]), 1], 
                   out.sample[1:length(out.sample[,2])-1, 2])
no.change.observed <- out.sample[2:length(out.sample[,2]), ]

mape.model1 <- mean(abs((as.numeric(no.change[,2]) - no.change.observed[,2]) 
                        / no.change.observed[,2])) * 100

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 1: Last-day no-change en Janvier 2017",
     xaxt = "n")
lines(no.change[,2] ~ as.Date(no.change[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(no.change[,1], "%Y-%m-%d"),
     labels = format(as.Date(no.change[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

################################################
# Mod?le 2: Taylor
################################################

library(forecast)

preds <- matrix(ncol=2, nrow=length(out.sample.ts))

for (n in 1:length(out.sample.ts)) {
  in.sample1 <- window(Yt.ts,
                       as.Date('2009-01-01') + n,
                       as.Date('2015-12-31') + n)
  in.sample1 <- msts(in.sample1,seasonal.periods=c(7,364))
  pred <- dshw(in.sample1, h = 1)
  preds[n,] <- c(as.character(as.Date(as.Date('2015-12-31') + n,
                                      format = "%Y-%m-%d")),
                 as.numeric(pred$mean[1]))
}

mape.model2 <- mean(abs((as.numeric(preds[,2]) - out.sample[,2]) 
                        / out.sample[,2])) * 100

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 2: Taylor en Janvier 2017",
     xaxt = "n")
lines(preds[,2] ~ as.Date(preds[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(preds[,1], "%Y-%m-%d"),
     labels = format(as.Date(preds[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))




################################################
# Mod?le 3: TBATS
################################################

library(forecast)

preds <- matrix(ncol=2, nrow=length(out.sample.ts))

for (n in 1:length(out.sample.ts)) {
  n <- 1
  in.sample1 <- window(Yt.ts,
                       as.Date('2009-01-01') + n,
                       as.Date('2015-12-31') + n)
  in.sample1 <- msts(in.sample1,seasonal.periods=c(7,365))
  fit.tbats <- tbats(in.sample1)
  pred <- forecast(fit.tbats, h=1)
  preds[n,] <- c(as.character(as.Date(as.Date('2015-12-31') + n,
                                      format = "%Y-%m-%d")),
                 as.numeric(pred$mean[1]))
}

mape.model3 <- mean(abs((as.numeric(preds[,2]) - out.sample[,2]) 
                        / out.sample[,2])) * 100

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 3: TBATS en Janvier 2017",
     xaxt = "n")
lines(no.change[,2] ~ as.Date(no.change[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(no.change[,1], "%Y-%m-%d"),
     labels = format(as.Date(no.change[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

################################################
# Mod?le 4: R?gression
################################################

library(forecast)

in.Tt <- window(Tt,
             as.Date('2009-01-01'),
             as.Date('2015-12-31'))

in.dow <- window(dow,
                 as.Date('2009-01-01'),
                 as.Date('2015-12-31'))

in.dayoff <- window(dayoff,
                    as.Date('2009-01-01'),
                    as.Date('2015-12-31'))

in.CDDt <- window(CDDt,
                  as.Date('2009-01-01'),
                  as.Date('2015-12-31'))

in.HDDt <- window(HDDt,
                  as.Date('2009-01-01'),
                  as.Date('2015-12-31'))



mreg <- lm(in.sample.ts ~ in.Tt + b=factor(in.dow) + factor(in.dayoff) +
              in.CDDt + lag(in.CDDt,1) + lag(in.CDDt,2) +
              in.HDDt + lag(in.HDDt,1) + lag(in.HDDt,2),
            x=T, y=T)

out.Tt <- window(Tt,
                as.Date('2016-01-01'),
                as.Date('2017-12-31'))

out.dow <- window(dow,
                  as.Date('2016-01-01'),
                  as.Date('2017-12-31'))

out.dayoff <- window(dayoff,
                     as.Date('2016-01-01'),
                     as.Date('2017-12-31'))

out.CDDt <- window(CDDt,
                   as.Date('2016-01-01'),
                   as.Date('2017-12-31'))

out.HDDt <- window(HDDt,
                   as.Date('2016-01-01'),
                   as.Date('2017-12-31'))

pred <- predict(mreg, newdata = data.frame(out.Tt, factor(out.dow), factor(out.dayoff), 
                                           out.CDDt,lag(out.CDDt,1),lag(out.CDDt,2),
                                           out.HDDt,lag(out.HDDt,1),lag(out.HDDt,2)))
reg.out.sample <- out.sample[-(1:2),2]

mape.model4 <- mean(abs((as.numeric(pred) - reg.out.sample) 
                        / reg.out.sample)) * 100

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 3: TBATS en Janvier 2017",
     xaxt = "n")
lines(no.change[,2] ~ as.Date(no.change[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(no.change[,1], "%Y-%m-%d"),
     labels = format(as.Date(no.change[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

# ACF/PACF du sample
library(astsa)
acf2(in.sample.ts)

################################################
# Mod?le 6: AR(5)
################################################

# Diagnostique de AR(5)
fit1 <- sarima(in.sample.ts, 5,0,0)

# Forecast de AR(5)
library(forecast)

preds <- matrix(ncol=2, nrow=length(out.sample.ts))

for (n in 1:length(out.sample.ts)) {
  in.sample1 <- window(Yt.ts,
                       as.Date('2009-01-01') + n,
                       as.Date('2015-12-31') + n)
  pred <- sarima.for(in.sample1, 5, 0, 0, 0, 0, 0, 0, n.ahead=1)
  preds[n,] <- c(as.character(as.Date(as.Date('2015-12-31') + n,
                                      format = "%Y-%m-%d")),
                 pred$pred[1])
}

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 6: AR(5) en Janvier 2017",
     xaxt = "n")
lines(preds[,2] ~ as.Date(preds[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(preds[,1], "%Y-%m-%d"),
     labels = format(as.Date(preds[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

mape.model6 <- mean(abs((as.numeric(preds[,2]) - out.sample[,2]) 
                        / out.sample[,2])) * 100

################################################
# Mod?le 7: ARIMA(5,1,0)
################################################

# Diagnostique de ARIMA(5,1,0)
library(astsa)
fit2 <- sarima(in.sample.ts, 5,1,0)

# Forecast de ARIMA(5,1,0)
library(forecast)

preds <- matrix(ncol=2, nrow=length(out.sample.ts))

for (n in 1:length(out.sample.ts)) {
  in.sample1 <- window(Yt.ts,
                       as.Date('2009-01-01') + n,
                       as.Date('2015-12-31') + n)
  pred <- sarima.for(in.sample1, 5, 1, 0, 0, 0, 0, 0, n.ahead=1)
  preds[n,] <- c(as.character(as.Date(as.Date('2015-12-31') + n,
                                      format = "%Y-%m-%d")),
                 pred$pred[1])
}

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 7: ARIMA(5,1,0) en Janvier 2017",
     xaxt = "n")
lines(preds[,2] ~ as.Date(preds[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(preds[,1], "%Y-%m-%d"),
     labels = format(as.Date(preds[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

mape.model7 <- mean(abs((as.numeric(preds[,2]) - out.sample[,2]) 
                        / out.sample[,2])) * 100

################################################
# Mod?le 8: SARIMA(5,1,0) (5,0,0) [7]
################################################

# Diagnostique de SARIMA(5,1,0) (5,0,0) [7]
library(astsa)
fit3 <- sarima(in.sample.ts, 5, 1, 0, 5, 0, 0, 7)

# Forecast de SARIMA(5,1,0) (5,0,0) [7]
library(forecast)

preds <- matrix(ncol=2, nrow=length(out.sample.ts))

for (n in 1:length(out.sample.ts)) {
  in.sample1 <- window(Yt.ts,
                       as.Date('2009-01-01') + n,
                       as.Date('2015-12-31') + n)
  pred <- sarima.for(in.sample1, 5, 1, 0, 5, 0, 0, 7, n.ahead=1)
  preds[n,] <- c(as.character(as.Date(as.Date('2015-12-31') + n,
                                      format = "%Y-%m-%d")),
                 pred$pred[1])
}

plot(Yt[,2] ~ as.Date(Yt[,1], "%Y-%m-%d"),
     type = "l",
     xlim = c(as.Date('2017-01-01', "%Y-%m-%d"),
              as.Date('2017-01-31', "%Y-%m-%d")),
     xlab = "Date",
     ylab = "Demande journali?re d'?lectricit?",
     main = "Mod?le 8: SARIMA(5,1,0) (5,0,0) [7] en Janvier 2017",
     xaxt = "n")
lines(preds[,2] ~ as.Date(preds[,1], "%Y-%m-%d"),
      type = "l",
      col = "blue")
axis(1,
     at = as.Date(preds[,1], "%Y-%m-%d"),
     labels = format(as.Date(preds[,1], "%Y-%m-%d"), "%d"))
legend("topright",legend=c("Observed","Predicted"),lty=c(1,1),
       col=c("black","blue"))

mape.model8 <- mean(abs((as.numeric(preds[,2]) - out.sample[,2]) 
                        / out.sample[,2])) * 100

