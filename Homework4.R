install.packages(c("dplyr","ggplot2","lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

sensorLog[4,2]

average <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)

# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)


#Prompt 1
#PLOT# examine precipitation using a bar plot
#MAY
ggplot(data=weather[weather$doy > 121 & weather$doy < 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

#JUNE
ggplot(data=weather[weather$doy > 152 & weather$doy < 181 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

#MARCH
ggplot(data=weather[weather$doy > 60 & weather$doy < 90 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

##Prompt 2

intv <- weather$dateF[1] %--% weather$dateF[2]

int_length(intv)

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]}

timeCheck900(weather$dateF)

#################################
#homework 4
#question 1
#exclude any precipitation occurring in temperatures below zero
ggplot(data=weather [weather$AirTemp > 0 & weather$XLevel < 2 & weather$YLevel < 2, ],
       aes(x=dateF ,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

weather$preci <- ifelse(weather$AirTemp < 0 | 
                          weather$XLevel > 2 |
                          weather$XLevel < -2 |
                          weather$YLevel < -2 | 
                          weather$YLevel > 2, 
                            # evaluate if the AirTemp is higher than 0 and between Xlevel 2 and Ylevel 2
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

length(weather$preci[is.na(weather$preci)])

#question 2
#add Battery Flag
weather$BatFlag <- ifelse(weather$BatVolt <= 8500, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#question 3
SolRadD <- function(x) {
  upper = (1000)
  lower = (0)
  length(x[x > upper | x < lower])
}

SolRadD(weather$SolRad)

airtemp <- function(x) {
  upper = (30)
  lower = (-20)
  length(x[x > upper | x < lower])
}

airtemp(weather$AirTemp)

#question 4
#winter air temperatures in Jan - Mar of 2021
ggplot(data=weather[weather$doy > 1 & weather$doy < 90 & weather$year == 2021, ],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  ggtitle("2021 Winter Air Temperatures")
  theme_classic()
