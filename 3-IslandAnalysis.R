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

##	MAK - INSTALL REQUIRED PACKAGES IF THEY ARE NOT INSTALLED
{my_packages <- c("tibble","tidyr","dplyr","lubridate","tidyverse","ggplot2","reshape2")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)}

#library(tibble)
#library(tidyr)
#library(dplyr)
#library(lubridate)
#library(tidyverse)
#library(ggplot2)
#library(reshape2)

#setting work directory
##	FRAGILE - only works on your computer, use relative path
#setwd('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/Data/R/')
#setwd('../Data')

####
#ISLAND.NAME <- 'Green'
#ISLAND.NAME <- 'North West'
ISLAND.NAME <- 'Raine'

if (ISLAND.NAME == 'Green') {
	ISLAND.FILE <- 'analysisData-Green.csfv'
}
if (ISLAND.NAME == 'Raine') {
	ISLAND.FILE <- 'analysisData-Raine.csv'
}
if (ISLAND.NAME == 'North West') {
	ISLAND.FILE <- 'analysisData-Northwest.csv'
}


#reading in island data
JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)

JoinIsland$SimpleDate <- as.Date(JoinIsland$SimpleDate)

lm.TideArea <- lm(Area..km2. ~ SLP_m, data = JoinIsland)
JoinIsland$ResidualArea <- residuals(lm.TideArea)


#reading in island data
ENSOdata <- read.csv(file = 'analysisData-ENSO.csv', row.names=1)
ENSOdata$ENSODateD <- as.Date(ENSOdata$ENSODateD)


summary(JoinIsland)
head(JoinIsland)



###	OPEN PLOT - LOG AREA AS PREDICTED BY SEA LEVEL

pdf(file=paste0('../Output/LASLP',ISLAND.NAME,'.pdf'), height='5', width='5')
par(oma=c(1,1,0,0), mar=c(4,4,1,1))

plot(JoinIsland$SLP_m, JoinIsland$LogArea, xlab = 'Tide Height (m)', ylab = 'Log10 Island Area (m\u00B2)', main = '', type='n')
lm.LASLP <- lm(LogArea ~ SLP_m, data = JoinIsland)
#abline(lm.RaineTide)
#summary(lm.RaineTide)
#mtext(paste0('r\u00B2 = ',round(summary(lm.RaineTide)$adj.r.squared,3)), side=1, adj=0.05, line=-2)

#predictions and CI for area x tidal height

pSLP <- seq(min(JoinIsland$SLP_m)-0.1,max(JoinIsland$SLP_m)+0.1,length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.LASLP, newdata= data.frame(SLP_m = pSLP), interval='prediction', level = 0.95) #prediction using lm.Raine
polygon(c(pSLP,rev(pSLP)),c(preds[,2],rev(preds[,3])), col = 'skyblue', border=NA)
lines(pSLP, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pSLP, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points
points(JoinIsland$SLP_m, JoinIsland$LogArea, pch = 19, cex=0.5)
abline(lm.LASLP)

predc <- predict.lm(lm.LASLP, newdata= data.frame(SLP_m = pSLP), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pSLP, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pSLP, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

rsqr <- round(summary(lm.LASLP)$adj.r.squared,3) #pulling adjusted r2 from summary of lm
mtext(paste0(ISLAND.NAME,' Island'), adj = 0.9, side=3, line=-2.5, cex=1.6) # margin text print r2
mtext(paste('r\u00b2 =', rsqr), adj = 0.9, side=3, line=-4) # margin text print r2

dev.off()


pdf(file=paste0('../Output/LASLP',ISLAND.NAME,'2.pdf'), height='5', width='5')
par(oma=c(1,1,0,0), mar=c(4,4,1,1))

#Figure 9A
#relationship between sea level and area
plot(LogArea ~ SLP_m, data = JoinIsland, xlab = 'Sea Level Prediction (m)', ylab = 'Log10 Area (m2)')
lm.RaineTide <- lm(LogArea ~ SLP_m, data = JoinIsland)
abline(lm.RaineTide)

pSLP <- seq(min(JoinIsland$SLP_m)-0.1,max(JoinIsland$SLP_m)+0.1,length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.RaineTide, newdata= data.frame(SLP_m = pSLP), interval='prediction', level = 0.95) #prediction using lm.Raine
polygon(c(pSLP,rev(pSLP)),c(preds[,2],rev(preds[,3])), col = 'skyblue', border=NA)
lines(pSLP, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pSLP, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points
points(JoinIsland$SLP_m, JoinIsland$LogArea, pch = 19, cex=0.5)
abline(lm.RaineTide)

predc <- predict.lm(lm.RaineTide, newdata= data.frame(SLP_m = pSLP), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pSLP, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pSLP, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

rsqr <- round(summary(lm.RaineTide)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.9, side=3, line=-2) # margin text print r2
#mtext(paste('A'), adj = -0.01, side=3, line=0.9) # margin text plot letter

dev.off()


###	END PLOT - LOG AREA AS PREDICTED BY SEA LEVEL




###	OPEN PLOT - TIME SERIES PANELS

pdf(file=paste0('../Output/TS',ISLAND.NAME,'.pdf'), paper='A4')
par(oma=c(2,2,1,0), mar=c(1,1,1,1))
par(mfrow=c(5,1))

#Figure 10
#area
plot(Area..km2. ~ SimpleDate, data = JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
#points(Area..km2. ~ SimpleDate, data = JoinIsland)
axis(2, las=1)
box(bty='L')
mtext(paste0(LETTERS[1],'. Island area (km\u00B2)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text
lm.RaineArea <- lm(Area..km2. ~SimpleDate, data = JoinIsland)
abline(lm.RaineArea, col = 'red')
rsqr <- round(summary(lm.RaineArea)$adj.r.squared,3) #pulling adjusted r2 from summary of lm

mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-1) # margin text print r2

mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2) # margin text


#proportional change in area
plot(DeltaAreaProp ~ SimpleDate, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[2],'. Proportional area change (%)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

lm.RaineProp <- lm(DeltaAreaProp~SimpleDate, data = JoinIsland)
abline(lm.RaineProp, col = 'red')
rsqr <- round(summary(lm.RaineProp)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-1) # margin text print r2

#tide height
plot(SLP_m  ~ SimpleDate, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[3],'. Tide Height (m)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

lm.TideHeight <- lm(SLP_m~SimpleDate, data = JoinIsland)
abline(lm.TideHeight, col = 'red')
rsqr <- round(summary(lm.TideHeight)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-1) # margin text print r2


#proportional change in tide height
plot(DeltaSLPProp ~ SimpleDate, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, bty='L', las=1)
axis(2, las=1)
abline(h=0)
mtext(paste0(LETTERS[4],'. Proportional tidal change (%)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

lm.TideProp <- lm(DeltaSLPProp~SimpleDate, data = JoinIsland)
abline(lm.TideProp, col = 'red')
rsqr <- round(summary(lm.TideProp)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-1) # margin text print r2

# ENSO
plot(SOI ~ ENSODateD, data=ENSOdata, type = 'b', axes=TRUE, ann=FALSE, bty='L', las=1)
#axis(2, las=1)
box(bty='L')
text(max(ENSOdata$ENSODateD),8,'La Nina')
abline(h=7, lty=2)
text(max(ENSOdata$ENSODateD),0,'Neutral')
abline(h=-7, lty=2)
text(max(ENSOdata$ENSODateD),-8,'El Nino')
mtext(paste0(LETTERS[5],'. Southern Oscilation Index'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

dev.off()
























plot(islandData$time, islandData$AreaTime, type = 'l')
abline(h = 0)
lm.AAT <- lm(AreaTime~time, data = islandData)
abline(lm.AAT)
summary(lm.AAT)
mtext(paste0("r\u00B2 = ",round(summary(lm.AAT)$adj.r.squared)), side=1, adj=0.9, line=-1)

#histogram, the spread of data --> magnitude doesn't change, mostly occurring in the centre etc of histogram
hist(islandData$AreaTime, breaks = 20)
quantile(islandData$AreaTime, na.rm = TRUE)
mean(islandData$AreaTime, na.rm = TRUE)
hist(islandData$AreaTime, breaks = seq(-7000, 6000, by = 500))

#plotting the ranges and means, medians etc/spread of the ISLAND data by month (seasonal look?/month look), but need to reorganise
#data to go from Jan to Dec, as it is currently organised in alphabetical order.
plot(as.factor(islandData$Month), islandData$AreaTime)
abline(h=0)

#attempt at reorganising months
islandData$Month <- factor(islandData$Month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
plot(as.factor(islandData$Month), islandData$AreaTime, xlab = 'Month', ylab = 'Range')


plot(Area..m2.~time, data = islandData)

##Trying to convert ISLAND time data to be more aligned with the tidal data we have, and rounding to nearest hour
#islandData$TideMinute <- format(round(islandData$rDate, units = 'mins'), format = '%d/%m/%Y %H:%M')






lm.LASLP.Residuals <- residuals(lm.LASLP)
plot(lm.LASLP)
plot(JoinIsland$time, lm.RaineTide.Residuals) #residuals over time
lm.RaineTideRes <- lm(lm.RaineTide.Residuals ~ JoinIsland$time)
summary(lm.RaineTideRes)
abline(lm.RaineTideRes)

lm.Model.LASLP <- lm(LogArea ~ SLP_m + time + DeltaArea + DeltaTime, data = JoinIsland) #+ DeltaSLP
summary(lm.Model.LASLP)

step(lm.Model.LASLP, direction = 'backward')
#step(lm.RaineTide2, direction = 'forward') #model may need redirection, respecification

lm.Model.DASLP <- lm(DeltaArea ~ SLP_m + time + DeltaTime, data = JoinIsland) #+ DeltaSLP
summary(lm.Model.DASLP)




#log area based on sea level
#Delta area, Delta time; sea level height.
#interpretation - July to September are most significant (though January is recognised as 0 categorical, need to convert into seasons)
# if splitting up it can be perciveded that Feb to April have greatest decrease, July to September greatest increase (Feb-April may become sig if converted toseasons)
lm.RaineMaster <- lm(LogArea ~ DeltaArea + DeltaTime + Month, data = JoinIsland)
summary(lm.RaineMaster)

#nothing below is significant when plotting delta area to delta time
lm.RaineMaster <- lm(DeltaArea ~ DeltaTime + Month, data = JoinIsland)
summary(lm.RaineMaster)

#splitting into seasons
lm.RaineMaster <- lm(LogArea ~ DeltaArea + DeltaTime + Season, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(LogArea ~ DeltaArea + DeltaTime + Season + SLP_m, data = JoinIsland)
summary(lm.RaineMaster)

#cyclone season
lm.RaineMaster <- lm(LogArea ~ DeltaArea + DeltaTime + Cyclone, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(LogArea ~ DeltaTime + Cyclone, data = JoinIsland)
summary(lm.RaineMaster)
plot(LogArea ~ as.factor(Cyclone), data = JoinIsland)
plot(LogArea ~ as.factor(Month), data = JoinIsland)
plot(LogArea ~ as.factor(Season), data = JoinIsland)

JoinIsland$idoy <- as.integer(format(JoinIsland$rDate.x, format = '%j'))
plot(LogArea ~ idoy, data = JoinIsland)
loess_Island <- loess(LogArea ~ idoy, data = JoinIsland)
plot(x=1:loess_Island$n,y=loess_Island$fitted, type='l') # come back, plot line, may need predict

#help Matt - should this next plot replace the time series etc that currently as Figure 10?

plot(islandData$time, islandData$AreaTime, type = 'l')
abline(h = 0)
lm.Raine <- lm(AreaTime~time, data = islandData)

pTime <- seq(min(islandData$time),max(islandData$time),length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.Raine, newdata= data.frame(time = pTime), interval='prediction', level = 0.95) #prediction using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pTime, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pTime, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points

predc <- predict.lm(lm.Raine, newdata= data.frame(time = pTime), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pTime, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pTime, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

pTime <- seq(min(islandData$time),max(islandData$time),length.out=100)
preds <- predict(lm.Raine, newdata= data.frame(time = pTime), interval='prediction')
polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col=rgb(1,0,1,0.3), border=NA)

########################## Raine TRIAL - Plotting tidal changes with time ###############################
#plotting Raine sea level predictions and date by line graph
plot(JoinIsland$rDate, JoinIsland$SLP_m, type = 'l')
#difference/change in SLP
#DeltaSLP <- diff(JoinIsland$SLP_m)
#as the first entry is NA because there is no difference in area for the first row, adding 0 into
#JoinIsland$DeltaSLP <- c(0, DeltaSLP)
#dividing area by time between
JoinIsland$HeightTime <- JoinIsland$DeltaSLP/JoinIsland$DeltaTime
plot(JoinIsland$time, JoinIsland$DeltaSLP, type = 'l')
#spread expected normal linear is 0, no change
abline(h = 0)
#linear model, change in area by time
lm.RaineTide <- lm(DeltaSLP~time, data = JoinIsland)
abline(lm.RaineTide)
summary(lm.RaineTide)

#changing both area and height to a proportion of previous
JoinIsland$DeltaAreaProp <- proportions(JoinIsland$DeltaArea)
JoinIsland$DeltaSLPProp <- proportions(JoinIsland$DeltaSLP)
DeltaAreaProp <- diff(JoinIsland$Area..m2.) / JoinIsland$Area..m2.[-length(JoinIsland$Area..m2.)] * 100
JoinIsland$DeltaAreaProp <- c(0, DeltaAreaProp)
DeltaSLPProp <- diff(JoinIsland$SLP_m) / JoinIsland$SLP_m[-length(JoinIsland$SLP_m)] * 100
JoinIsland$DeltaSLPProp <- c(0, DeltaSLPProp)

##plotting Raine area, then also plotting tidal info
plot(JoinIsland$time, JoinIsland$DeltaArea, type = 'l')
#spread expected normal linear is 0, no change
abline(h = 0)
#linear model, change in area by time
lm.Raine <- lm(DeltaArea~time, data = JoinIsland)
abline(lm.Raine, col = 'red')
lines(JoinIsland$time, JoinIsland$HeightTime, type = 'l', col = 'blue')
#above doesn't work, two different axis ranges --> convert to proportion

#plotting proportions
plot(JoinIsland$time, JoinIsland$DeltaAreaProp, type = 'l')
#spread expected normal linear is 0, no change
abline(h = 0)
#linear model, change in area by time
lm.RaineProp <- lm(DeltaAreaProp~time, data = JoinIsland)
abline(lm.RaineProp, col = 'red')
#lines(JoinIsland$time, JoinIsland$DeltaSLPProp, type = 'l', col = 'blue')
lm.SLPProp <- lm(DeltaSLPProp~time, data = JoinIsland)
abline(lm.SLPProp, col = 'blue')

##stacking plots



par(mfrow=c(1,1))


plot(JoinIsland$SimpleDate, JoinIsland$Area..m2., type = 'l', xlab = '', ylab = 'Area (m2)')

#segments
#segments(ENSO$Simpledate, ENSO$SOI, ENSO$simple date, 0, lwd=3, col = 'purple')

abline(h=0)
lm.RaineArea <- lm(Area..m2.~SimpleDate, data = JoinIsland)
abline(lm.RaineArea, col = 'red')
rsqr <- round(summary(lm.RaineArea)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-2) # margin text print r2
mtext(paste('Area'), adj = 1, side=3, line=0) # margin text
mtext(paste('A'), adj = -0.01, side=3, line=0.9) # margin text plot letter
polygon(x=c(JoinIsland$SimpleDate[JoinIsland$SOI >= 7]), col = rgb(0.4,0.4,0.4,0.25),border=NA)
abline(v=c(JoinIsland$SimpleDate[JoinIsland$SOI >= 7]), col = 'green')
polygon(x=c(JoinIsland$SimpleDate[JoinIsland$SOI <= -7]), col = rgb(0.4,0.4,0.4,0.25),border=NA)
abline(v=c(JoinIsland$SimpleDate[JoinIsland$SOI <= -7]), col = 'purple')

#proportional change in area
plot(JoinIsland$SimpleDate, JoinIsland$DeltaAreaProp, type = 'l', xlab = '', ylab = 'Proportional Change (%)')
abline(h=0)
lm.RaineProp <- lm(DeltaAreaProp~SimpleDate, data = JoinIsland)
abline(lm.RaineProp, col = 'red')
rsqr <- round(summary(lm.RaineProp)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=-2) # margin text print r2
mtext(paste('Area'), adj = 1, side=3, line=0) # margin text
mtext(paste('B'), adj = -0.01, side=3, line=0.9) # margin text plot letter
polygon(x=c(JoinIsland$SimpleDate[JoinIsland$SOI >= 7]),border=NA)
abline(v=c(JoinIsland$SimpleDate[JoinIsland$SOI >= 7]), col = 'green')
polygon(x=c(JoinIsland$SimpleDate[JoinIsland$SOI <= -7]), col = rgb(0.4,0.4,0.4,0.25),border=NA)
abline(v=c(JoinIsland$SimpleDate[JoinIsland$SOI <= -7]), col = 'purple')

par(mfrow=c(1,1))

############
#lm sea level prediction

lm.Raine_SLP <- lm(SLP_m~SimpleDate, data = JoinIsland)
summary(lm.Raine_SLP)
# lm change in area
lm.RaineDeltaArea <- lm(DeltaArea~SimpleDate, data = JoinIsland)
summary(lm.RaineDeltaArea)

#days period between 
lm.DeltaDays <- lm(DeltaTime~SimpleDate, data = JoinIsland)
summary(lm.DeltaDays)

library(plotrix)
#average m by year
tapply(JoinIsland$Area..m2., JoinIsland$Year, summary)
#Raine area standard error
#std.error(JoinIsland$Area..m2.)
# 2016 had the highest average, looking at mean 2016
Raine_2016 <- JoinIsland[(JoinIsland$Year == '2016'), ]
lm.RaineArea2016 <- lm(Area..m2.~SimpleDate, data = Raine_2016)
summary(lm.RaineArea2016)

#average change m by year
tapply(JoinIsland$DeltaArea, JoinIsland$Year, summary)
