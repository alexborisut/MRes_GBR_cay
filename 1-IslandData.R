################################  FILE LICENSE  ################################
#
#	This file is copyright (C) 2024 Alex Borisut
#
#	This program is free software; you can redistribute it and/or modify it 
#	under the terms of version 3 the GNU General Public License as published 
#	by the Free Software Foundation.
#
#	This program is distributed in the hope that it will be useful, but WITHOUT
#	ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for 
#	more details.
#
#	To view a copy of the license go to:
#	http://www.gnu.org/licenses/licenses.html#GPL
#	
################################################################################

##
##	CODE USED FOR THE MASTER RESEARCH THESIS
##
##
##	ISLAND DATA IMPORT / MERGING / IMPLEMENT CLEANING / SUBSETTING / GROUPING
##########################################################################################

PATH <- '../'
PATH.OUT <- paste0(PATH,'Data','/R/')

if(!paste(PATH.OUT) %in% list.dirs(PATH)) dir.create(PATH.OUT)#

##	MAK - INSTALL REQUIRED PACKAGES IF THEY ARE NOT INSTALLED
{my_packages <- c("tibble","tidyr","dplyr","lubridate","tidyverse","ggplot2","reshape2")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)}

#library(tibble)
#library(tidyr)
#library(dplyr)
library(lubridate)
#library(tidyverse)
#library(ggplot2)
#library(reshape2)

#setting work directory
##	FRAGILE - only works on your computer, use relative path
setwd('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/Data/R/')
#setwd('../Data')

#### RUN THIS FILE FOR EACH ISLAND TO GET THE DATA FILE USED IN THE ANALYSIS SCRIPTS.
####	
#ISLAND.NAME <- 'Raine'
#ISLAND.NAME <- 'North West'
ISLAND.NAME <- 'Green'

ISLAND.FILE <- 'Green_Raine_Northwest_RAW.csv'

if (ISLAND.NAME == 'Green') {
	TIDE.FILE <- 'Green_Tidal_Data_Compiled.csv'
}
if (ISLAND.NAME == 'Raine') {
	TIDE.FILE <- 'Raine_Tidal_Data_Compiled.csv'
}
if (ISLAND.NAME == 'North West') {
	TIDE.FILE <- 'Heron_Tidal_Data_Compiled.csv'
}

# reading in island data
Raw_Data <- read.csv(file = ISLAND.FILE)

unique(Raw_Data$ï..Island)
summary(Raw_Data)

#as normal date/time information is difficult to read in R, convert the time and date to char.
Raw_Data$rDate <- strptime(Raw_Data$AEST.start.date.time, format = '%d/%m/%Y %I:%M:%S %p')
#temporary, only includes start date time and rData in this 
temp <- Raw_Data[, c('AEST.start.date.time', 'rDate')]
#creating a new column to only take years from start
temp$iYear <- as.integer(format(temp$rDate, format = '%Y'))
#creating a new column to take day and month from start
temp$idoy <- as.integer(format(temp$rDate, format = '%j'))
##temp$time <- as.integer(format(temp$rDate, format = '%H'))
#combining years and days/time to know how many days from start
temp$time <- (temp$iYear - min(temp$iYear)) * 365 + temp$idoy
# mak - this shoud also work and might be more robust?
temp$daysSince <- as.integer((temp$rDate - min(temp$rDate))/(60*60*24))

#adding in temp time to the dataset
Raw_Data$time <- temp$time

# SUBSET FOR ONLY ONE ISLAND
islandData <- Raw_Data[(Raw_Data$ï..Island == ISLAND.NAME), ]

dim(islandData)
summary(islandData)
head(islandData)

#adding new column to do simple dates for lm
islandData$SimpleDate <- as.Date(islandData$rDate, format = '%Y-%M-%D')

#difference/change in island area between photos
DeltaArea <- diff(islandData$Area..m2.)
#as the first entry is NA because there is no difference in area for the first row, adding 0 into
islandData$DeltaArea <- c(0, DeltaArea)

#difference/change in time between photos (in days)
DeltaTime <- diff(islandData$time)
#as the first entry is NA because there is no difference in time in the first row, adding 0
islandData$DeltaTime <- c(0, DeltaTime)

#dividing area by time between
islandData$AreaTime <- islandData$DeltaArea/islandData$DeltaTime

# mak - move this all to the top.
islandData$LogArea <- log10(islandData$Area..m2.)

# set seasons
islandData$Season <- 'Summer'
islandData[(islandData$Month %in% c('March', 'April', 'May')),'Season'] <- 'Fall'
islandData[(islandData$Month %in% c('June', 'July', 'August')),'Season'] <- 'Winter'
islandData[(islandData$Month %in% c('September', 'October', 'November')),'Season'] <- 'Spring'

# set cyclone season
islandData$Cyclone <- 'Yes' 
islandData[(islandData $Month %in% c('May','June','July','August','September','October')),'Cyclone'] <- 'No'

# set tide matching time
islandData$TideHour <- format(round(islandData$rDate, units = 'hours'), format = '%d/%m/%Y %H:%M')


#reading in ISLAND tidal data, and converting to same format as above ^
#reading in data
Raw_TData <- read.csv(file = TIDE.FILE)
colnames(Raw_TData) <- c('Date_Time','SLP_m')
summary(Raw_TData)

#converting tidal date/time into the same format as the ISLAND data file
Raw_TData$rDate <- strptime(Raw_TData$Date_Time, format = '%d/%m/%Y %H:%M')
Raw_TData$TideHour <- format(round(Raw_TData$rDate, units = 'hours'), format = '%d/%m/%Y %H:%M')
TideData <- Raw_TData[,c('TideHour','SLP_m')]
summary(TideData)

# inner join tables - add tide data to island data
#JoinIsland <- inner_join(islandData, TideData, by = 'TideHour')
JoinIsland <- merge(islandData, TideData, by = 'TideHour', all.x=TRUE)
head(JoinIsland)

# change in tide
DeltaSLP <- diff(JoinIsland$SLP_m)
#as the first entry is NA because there is no difference in area for the first row, adding 0 into
JoinIsland$DeltaSLP <- c(0, DeltaSLP)

#changing both area and height to a proportion of previous
JoinIsland$DeltaAreaProp <- proportions(JoinIsland$DeltaArea)
JoinIsland$DeltaSLPProp <- proportions(JoinIsland$DeltaSLP)

lm.TideArea <- lm(Area..km2. ~ SLP_m, data = JoinIsland)
JoinIsland$ResidualArea <- residuals(lm.TideArea)


##	ENSO DATA
#####################

JoinIsland$ENSODate <- strptime(JoinIsland$rDate, format = '%Y-%m-%d')
JoinIsland$ENSODate <- substring((JoinIsland$ENSODate),0,7)

#reading in enso data sourced from BOM website
Raw_Data <- read.csv(file = 'ENSO_Monthly.csv')
#Raw_Data$Year <- substr(Raw_Data$Month, start = 1, stop = 4) %>% as.numeric()
#Raw_Data$Month <- substr(Raw_Data$Month, start = 5, stop = 6) %>% as.numeric()
Raw_Data$Year <- as.numeric(substr(Raw_Data$ï..Month, start = 1, stop = 4))
Raw_Data$Month <- as.numeric(substr(Raw_Data$ï..Month, start = 5, stop = 6))
#Raw_Data$ENSODate <- paste0(Raw_Data$Year, "/", Raw_Data$Month)
#Raw_Data$ENSODate <- as.Date(with(Raw_Data, paste0(Year, Month, sep='-')), '%Y-%m')
Raw_Data$ENSODateD <- with(Raw_Data, ym(sprintf('%04d%02d', Year, Month)))
Raw_Data$ENSODate <- strptime(Raw_Data$ENSODateD, format = '%Y-%m-%d')
Raw_Data$ENSODate <- format(round(Raw_Data$ENSODate, units = 'months'), format = '%Y-%m')

#separating neutral, el nino and la nina events
ENSO <- Raw_Data
ENSO$Event <- 'Neutral'
ENSO[(ENSO$SOI >= 7), 'Event'] <- 'La Nina'
ENSO[(ENSO$SOI <= -7), 'Event'] <- 'El Nino'

#subset only ENSO data for the study period
ENSO2 <- ENSO 
ENSO2 <- ENSO[ENSO$ENSODate > min(JoinIsland$ENSODate),]

#joining ENSO information to Raine information
JoinIsland <- merge(x = JoinIsland, y = ENSO[ , c('ENSODate', 'SOI', 'Event')], by = 'ENSODate', all.x = TRUE)


head(JoinIsland)
summary(JoinIsland)
dim(JoinIsland)

write.csv(JoinIsland, file=paste0('./analysisData-',ISLAND.NAME,'.csv'))
write.csv(ENSO2, file=paste0('./analysisData-ENSO.csv'))

