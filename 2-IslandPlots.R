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

PATH <- '../'
PATH.OUT <- paste0(PATH,'Output','/')
if(!paste(PATH.OUT) %in% list.dirs(PATH)) dir.create(PATH.OUT)#

##	MAK - INSTALL REQUIRED PACKAGES IF THEY ARE NOT INSTALLED
{my_packages <- c("tibble","tidyr","dplyr","lubridate","tidyverse","ggplot2","reshape2", "sp")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)}

#library(tibble)
#library(tidyr)
#library(dplyr)
#library(lubridate)
#library(tidyverse)
#library(ggplot2)
#library(reshape2)
library(sp)

#setting work directory
##	FRAGILE - only works on your computer, use relative path
setwd('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/Data/R/')
#setwd('../Data')

####
#ISLAND.NAME <- 'Green'
#ISLAND.NAME <- 'Raine'
ISLAND.NAME <- 'North West'

if (ISLAND.NAME == 'Green') {
  ISLAND.FILE <- 'analysisData-Green.csv'
  Lat <- -1 * (16 + 45.5/60)
  Lng <- 145 + 58.5/60
}
if (ISLAND.NAME == 'Raine') {
  ISLAND.FILE <- 'analysisData-Raine.csv'
  Lat <- -1 * (11 + 36/60)
  Lng <- 144 + 1/60
}
if (ISLAND.NAME == 'North West') {
  ISLAND.FILE <- 'analysisData-North West.csv'
  Lat <- -1 * (23 + 18/60)
  Lng <- 151 + 45/60
}


#reading in island data
JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)

JoinIsland$SimpleDate <- as.Date(JoinIsland$SimpleDate)

JoinIsland$Month <- factor(JoinIsland$Month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

JoinIsland$Season <- factor(JoinIsland$Season, levels = c('Spring','Summer','Fall','Winter'))

JoinIsland$Cyclone <- factor(JoinIsland$Cyclone, levels = c('Yes','No'))




#reading in island data
ENSOdata <- read.csv(file = 'analysisData-ENSO.csv', row.names=1)
ENSOdata$ENSODateD <- as.Date(ENSOdata$ENSODateD)

# READ IN CYCLONE DATA
cycloneData <- read.csv(file = 'IDCKMSTM0S_cleaned_AEST.csv')
cycloneData <- cycloneData[(cycloneData$Year > '2014'), ]

cycloneData$distance <- spDistsN1(pts=as.matrix(cycloneData[,c('LON','LAT')]),pt=c(Lng,Lat),longlat=TRUE)

cyclone2 <- cycloneData[,c('ï..NAME','Date_Time_AEST','SURFACE_CODE','LAT','LON','distance')]
cyclone2$rDate <- strptime(cyclone2$Date_Time_AEST, format = '%Y/%m/%d %I:%M:%S %p')
cyclone2$SimpleDate <- as.Date(cyclone2$rDate, format = '%Y-%M-%D')
cyclone2$iYear <- as.integer(format(cyclone2$rDate, format = '%Y'))
cyclone2$idoy <- as.integer(format(cyclone2$rDate, format = '%j'))
cyclone2 $time <- (cyclone2$iYear - min(cyclone2$iYear)) * 365 + cyclone2 $idoy

summary(JoinIsland)
head(JoinIsland)



###	PLOT FUNCTIONS - SAVE A BUNCH OF REPEATED CODE
##########################################################################################
tsPredPlot  <- function(model, interval='prediction', level=0.95, plotLines=TRUE, lty=2, lineCol='black', plotArea=FALSE,areaColor) {
  
  xMin <- min(model$model[,2])*.9
  xMax <- max(model$model[,2])*1.1
  xDur <- xMax-xMin
  xSeq <- seq(xMin-xDur*0.05,xMax+xDur*0.05,length.out=100)
  new <- data.frame(xSeq)
  colnames(new) <- names(model$model)[2]
  preds <- predict.lm(model, newdata= new, interval=interval, level = level)
  if (plotArea == TRUE)
    polygon(c(xSeq,rev(xSeq)),c(preds[,'lwr'],rev(preds[,'upr'])), col= areaColor, border=NA)
  if (plotLines == TRUE) {
    lines(xSeq, preds[,'lwr'], lty = lty, col = lineCol) #lower prediction interval, outlier points
    lines(xSeq, preds[,'upr'], lty = lty, col = lineCol) # upper prediction interval, outlier points
  }	
}

tsPlotStats  <- function(model, r2, pv) {
  
  if (pv==TRUE) {
    fStat <- summary(model)$fstatistic
    pVal <- 1 - round(pf(q=fStat[1], df1=fStat[2], df2=fStat[3]),3)
    pValText <- paste('p =', pVal)
    if (pValText  == 'p = 0') pValText <- 'p < 0.001'
    mtext(pValText, adj = 0.98, side=3, line=-1.1) # margin text print r2
  }
  
  if ((pVal < 0.1) && (r2==TRUE)) {
    rsqr <- round(summary(model)$adj.r.squared,3) #pulling adjusted r2 from summary of lm
    if (rsqr < 0) {
      mtext(('r\u00b2 < 0'), adj = 0.98, side=3, line=0) # margin text print r2
    } else {
      mtext(paste('r\u00b2 =', rsqr), adj = 0.98, side=3, line=0) # margin text print r2
    }
  }
  return(pVal)
}

tsBackgroundENSO <- function(yRange, ENSOdata, bgCol=rgb(0,1,0,0.2)) {
  
  yRng <- yRange[2] - yRange[1]
  
  ePoly <- data.frame(upr=rep(0,nrow(ENSOdata)), lwr=rep(0,nrow(ENSOdata)), x = ENSOdata$ENSODateD)
  ePoly[(ENSOdata$SOI > 7),'upr'] <- yRange[2] + yRng * 0.2
  ePoly[(ENSOdata$SOI < -7),'lwr'] <- yRange[1] - yRng * 0.2
  
  pp <- data.frame(upr=rep(0,2*nrow(ePoly)-1), lwr=rep(0,2*nrow(ePoly)-1), x=rep(0,2*nrow(ePoly)-1))
  x <- 2
  pp[1,] <- ePoly[1,]
  for (r in 2:nrow(ePoly)) {
    pp[x,] <- ePoly[r,]
    pp[x,'x'] <- ePoly[(r-1),'x']
    x <- x + 1
    pp[x,] <- ePoly[r,]
    pp[x,'x'] <- ePoly[r,'x']
    x <- x + 1
  }
  
  polygon(c(pp$x,rev(pp$x)), c(pp$upr,rev(pp$lwr)), col=bgCol)
  
}

#yRange <- range(ENSOdata$SOI)

tsBackgroundCyclones <- function(yRange, cycloneData, timeColumn, col='seagreen') {
  
  yRng <- yRange[2] - yRange[1]
  
  y0 <- rep((yRange[1] - yRng*0.1),nrow(cycloneData))
  y1 <- rep((yRange[2] + yRng*0.1),nrow(cycloneData))
  
  y0 <- 5
  y1 <- -5
  
  
  segments (x0= cycloneData[, timeColumn],y0=y0,x1= cycloneData[, timeColumn],y1=y1, col=col)
  
}


###	OPEN PLOT - LOG AREA AS PREDICTED BY SEA LEVEL
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'-LASLP.pdf'), height=5, width=5)
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


###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'LASLP-2.pdf'), height=5, width=5)
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

###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'-TS.pdf'), paper='A4', width=6, height=8)
par(oma=c(2,2,1,0), mar=c(1,1,1,1))
par(mfrow=c(5,1))

#Figure 10
# area by time
#######################################################
lm.RaineArea <- lm(Area..km2. ~ time, data = JoinIsland)
plot(Area..km2. ~ time, data = JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
#points(Area..km2. ~ SimpleDate, data = JoinIsland)
tsBackgroundCyclones(range(JoinIsland$Area..km2.), cyclone2[(cyclone2$distance < 250),],'time')
axis(2, las=1)
box(bty='L')
mtext(paste0(LETTERS[1],'. Island area (km\u00B2)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

abline(h=mean(JoinIsland$Area..km2.), lty=1, col='forestgreen')
pVal <- tsPlotStats(lm.RaineArea, r2=TRUE, pv=TRUE)

abline(lm.RaineArea, col = 'red')
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}
tsPredPlot(lm.RaineArea,'prediction',0.96,TRUE,2,'blue', plotArea, rgb(0,0,0,0.3))
tsPredPlot(lm.RaineArea,'confidence',0.96,TRUE,2,'red')

mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2) # margin text


#proportional change in area
#######################################################
lm.RaineProp <- lm(DeltaAreaProp ~ time, data = JoinIsland)
plot(DeltaAreaProp ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
tsBackgroundCyclones(range(JoinIsland$DeltaAreaProp), cyclone2[(cyclone2$distance < 250),],'time')

axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[2],'. Proportional area change (%)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

abline(lm.RaineProp, col = 'red')

pVal <- tsPlotStats(lm.RaineProp, r2=TRUE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}
tsPredPlot(lm.RaineProp,'prediction',0.96,TRUE,2,'blue', plotArea, rgb(0,0,0,0.3))
tsPredPlot(lm.RaineProp,'confidence',0.96,TRUE,2,'red')

abline(h=mean(JoinIsland$DeltaAreaProp), lty=1, col='forestgreen')


#tide height
#######################################################
lm.TideHeight <- lm(SLP_m ~ time, data = JoinIsland)
plot(SLP_m  ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[3],'. Tide Height (m)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

abline(lm.TideHeight, col = 'red')

pVal <- tsPlotStats(lm.TideHeight, r2=TRUE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}
tsPredPlot(lm.TideHeight,'prediction',0.96,TRUE,2,'blue', plotArea, rgb(0,0,0,0.3))
tsPredPlot(lm.TideHeight,'confidence',0.96,TRUE,2,'red')
abline(h=mean(JoinIsland$SLP_m), lty=1, col='forestgreen')


#proportional change in tide height
#######################################################
lm.TideProp <- lm(DeltaSLPProp ~ time, data = JoinIsland)
plot(DeltaSLPProp ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, bty='L', las=1)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[4],'. Proportional tidal change (%)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

abline(lm.TideProp, col = 'red')

pVal <- tsPlotStats(lm.TideProp, r2=TRUE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}

tsPredPlot(lm.TideProp,'prediction',0.96,TRUE,2,'blue',plotArea, rgb(0,0,0,0.3))
tsPredPlot(lm.TideProp,'confidence',0.96,TRUE,2,'red')
abline(h=mean(JoinIsland$DeltaSLPProp), lty=1, col='forestgreen')



# ENSO
#######################################################
plot(SOI ~ ENSODateD, data=ENSOdata, type = 'b', axes=TRUE, ann=FALSE, bty='L', las=1, pch=19, cex=0.7)
#axis(2, las=1)
box(bty='L')
text(max(ENSOdata$ENSODateD),10,'La Ni\u00F1a')
abline(h=7, lty=2)
#text(max(ENSOdata$ENSODateD),0,'Neutral')
abline(h=-7, lty=2)
text(max(ENSOdata$ENSODateD)-600,-10,'El Ni\u00F1o')
mtext(paste0(LETTERS[5],'. Southern Oscilation Index'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

tsBackgroundENSO(range(ENSOdata$SOI), ENSOdata, bgCol=rgb(0,0,0,0.1))

tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 250),],'time')

dev.off()
dev.off()

###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/Fig10-',ISLAND.NAME,'-TS2.pdf'), paper='A4', height=8, width=6)
par(oma=c(2,4.5,1,0), mar=c(1,1,1,1))
par(mfrow=c(4,1))


JoinIsland <- JoinIsland[order(JoinIsland$time),]

ylabLine <- 3.75

#Figure 10
# area by time
#######################################################
lm.RaineArea <- lm(Area..km2. ~ time, data = JoinIsland)
yRng <- range(JoinIsland$Area..km2.)
yRng[1] <- yRng[1] - (max(yRng) -  abs(min(yRng)))*0.1
yRng[2] <- yRng[2] + (max(yRng) -  abs(min(yRng)))*0.1

plot(Area..km2. ~ time, data = JoinIsland, type = 'b', axes=FALSE, ann=FALSE, ylim= yRng)
#points(Area..km2. ~ SimpleDate, data = JoinIsland)
tsBackgroundCyclones(range(JoinIsland$Area..km2.), cyclone2[(cyclone2$distance < 250),],'time')
axis(2, las=1)
box(bty='L')
mtext(paste0(LETTERS[1],'. Raw Island Area'), adj = 0.05, side=3, line=0, cex=1.2) # margin text
mtext('Raw area (km\u00B2)', side=2, line=ylabLine)

abline(h=mean(JoinIsland$Area..km2.), lty=1, col='forestgreen')
pVal <- tsPlotStats(lm.RaineArea, r2=TRUE, pv=TRUE)

abline(lm.RaineArea, col = 'red')
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}
tsPredPlot(lm.RaineArea,'prediction',0.96,TRUE,2,'blue')
tsPredPlot(lm.RaineArea,'confidence',0.96,TRUE,2,'red', plotArea, rgb(0,0,0,0.2))

mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2) # margin text


#tide height
#######################################################
lm.TideHeight <- lm(SLP_m ~ time, data = JoinIsland)
yRng <- range(JoinIsland$SLP_m)
yRng[1] <- yRng[1] - (max(yRng) -  abs(min(yRng)))*0.1
yRng[2] <- yRng[2] + (max(yRng) -  abs(min(yRng)))*0.1

plot(SLP_m  ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, ylim= yRng)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[2],'. Tide Height'), adj = 0.05, side=3, line=0, cex=1.2) # margin text
mtext('Tide Height (m)', side=2, line=ylabLine)

abline(lm.TideHeight, col = 'red')

pVal <- tsPlotStats(lm.TideHeight, r2=TRUE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}
tsPredPlot(lm.TideHeight,'prediction',0.96,TRUE,2,'blue')
tsPredPlot(lm.TideHeight,'confidence',0.96,TRUE,2,'red', plotArea, rgb(0,0,0,0.2))
abline(h=mean(JoinIsland$SLP_m), lty=1, col='forestgreen')

tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 250),],'time')

#Area tide residuals
#######################################################
yRng <- range(JoinIsland$ResidualArea)
yRng[1] <- yRng[1] - (max(yRng) -  abs(min(yRng)))*0.1
yRng[2] <- yRng[2] + (max(yRng) -  abs(min(yRng)))*0.1
lm.TideProp <- lm(ResidualArea ~ time, data = JoinIsland)
plot(ResidualArea ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, bty='L', las=1, ylim= yRng)
axis(2, las=1)
box(bty='L')
abline(h=0)
mtext(paste0(LETTERS[3],'. Tidal adjusted area'), adj = 0.05, side=3, line=0, cex=1.2) # margin text
mtext('Adjusted area (km\u00B2)', side=2, line=ylabLine)

abline(lm.TideProp, col = 'red')

pVal <- tsPlotStats(lm.TideProp, r2=TRUE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}

tsPredPlot(lm.TideProp,'prediction',0.96,TRUE,2,'blue')
tsPredPlot(lm.TideProp,'confidence',0.96,TRUE,2,'red',plotArea, rgb(0,0,0,0.2))
abline(h=mean(JoinIsland$DeltaSLPProp), lty=1, col='forestgreen')

tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 250),],'time')
#tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 100),],'time','pink')

#Area tide residuals FD
#######################################################
#FDTR <- c(0,diff(tideAreaResiduals))
#yRng <- range(FDTR)
#yRng[1] <- yRng[1] - (max(yRng) -  abs(min(yRng)))*0.1
#yRng[2] <- yRng[2] + (max(yRng) -  abs(min(yRng)))*0.1
#lm.FDTR <- lm(FDTR ~ time, data = JoinIsland)
#plot(FDTR ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, bty='L', las=1, ylim= yRng)
#axis(2, las=1)
#box(bty='L')
#abline(h=0)
#mtext(paste0(LETTERS[4],'. Tidal adjusted area first differences (km2)'), adj = 0.05, side=3, line=0, cex=1.2) # margin text

#abline(lm.FDTR, col = 'red')

#pVal <- tsPlotStats(lm.FDTR, r2=TRUE, pv=TRUE)
#if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}

#tsPredPlot(lm.FDTR,'prediction',0.96,TRUE,2,'blue')
#tsPredPlot(lm.FDTR,'confidence',0.96,TRUE,2,'red',plotArea, rgb(0,0,0,0.2))
#abline(h=mean(FDTR), lty=1, col='forestgreen')

#tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 250),],'time')


# ENSO
#######################################################
plot(SOI ~ ENSODateD, data=ENSOdata, type = 'b', axes=TRUE, ann=FALSE, bty='L', las=1, pch=19, cex=0.7)
#axis(2, las=1)
box(bty='L')
text(max(ENSOdata$ENSODateD),10,'La Ni\u00F1a')
abline(h=7, lty=2)
#text(max(ENSOdata$ENSODateD),0,'Neutral')
abline(h=-7, lty=2)
text(max(ENSOdata$ENSODateD)-600,-10,'El Ni\u00F1o')
mtext(paste0(LETTERS[4],'. Southern Oscilation Index'), adj = 0.05, side=3, line=0, cex=1.2) # margin text
mtext('Southern Oscilation Index', side=2, line=ylabLine)

tsBackgroundENSO(range(ENSOdata$SOI), ENSOdata, bgCol=rgb(0,0,0,0.1))

tsBackgroundCyclones(range(ENSOdata$SOI), cyclone2[(cyclone2$distance < 250),],'time')

dev.off()



#plot(JoinIsland$time, JoinIsland$AreaTime, type = 'l')
#abline(h = 0)
#lm.AAT <- lm(AreaTime~time, data = JoinIsland)
#abline(lm.AAT)
#summary(lm.AAT)
#mtext(paste0("r\u00B2 = ",round(summary(lm.AAT)$adj.r.squared,3)), side=1, adj=0.9, line=-1)



###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'-AD-histogram.pdf'), paper='A4')
par(oma=c(2,2,1,0), mar=c(2,2,1,1))

#histogram, the spread of data --> magnitude doesn't change, mostly occurring in the centre etc of histogram
hist(JoinIsland$AreaTime, breaks = 20, ann=FALSE, las=1)
a <- quantile(JoinIsland$AreaTime, na.rm = TRUE)
mean(JoinIsland$AreaTime, na.rm = TRUE)
mtext('Island area change (m\u00B2 / day)', adj=0.5, line=2.5, side=1)

mtext(paste0('median: ',round(a[3],0)), side=3, line=-3, adj=0.05)
mtext(paste('IQR:',round(a[2],0),'to',round(a[4],0)), side=3, line=-4, adj=0.05)

#hist(JoinIsland$AreaTime, breaks = seq(-7000, 6000, by = 500))
mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2) # margin text
dev.off()



###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'-AD-Month.pdf'), paper='A4')
par(oma=c(3,3,1,0), mar=c(1,1,1,1))


#plotting the ranges and means, medians etc/spread of the ISLAND data by month (seasonal look?/month look), but need to reorganise
#data to go from Jan to Dec, as it is currently organised in alphabetical order.
plot(as.factor(JoinIsland$Month), JoinIsland$AreaTime, axes=FALSE)
axis(2)
mtext('Change in island area (m\u00B2 / day)',side=2, line=2.5, las=0)
box(bty='L')
axis(1, at= 1:12, labels=month.abb)
mtext('Month',side=1, line=2.5, las=0)
points(as.factor(JoinIsland$Month), JoinIsland$AreaTime, pch=19, cex=0.5)
abline(h=0)

mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2) # margin text
dev.off()


###	OPEN PLOT -
##########################################################################################
pdf(file=paste0('../Output/',ISLAND.NAME,'-ResidualArea-Panel.pdf'), paper='A4')
par(oma=c(3,4,2,0), mar=c(2,1,2,1), las=1)
par(mfcol=c(3,1))

plot(ResidualArea ~ Month, data = JoinIsland, ann=FALSE, axes=FALSE)
points(JoinIsland$Month, JoinIsland$LogArea, pch=19, cex=0.5)
box(bty='L')
axis(2)
axis(1, at= 1:12, labels=month.abb)
mtext(paste0(LETTERS[1],'. Month'), side=3, line=0, adj=0.05)

plot(ResidualArea ~ Season, data = JoinIsland, ann=FALSE, axes=FALSE)
points(JoinIsland$Season, JoinIsland$LogArea, pch=19, cex=0.5)
box(bty='L')
axis(2)
#axis(1)
axis(1, at=1:4, labels=unique(JoinIsland$Season))
mtext(paste0(LETTERS[2],'. Season'), side=3, line=0, adj=0.05)

plot(ResidualArea ~ Cyclone, data = JoinIsland, ann=FALSE, axes=FALSE)
points(JoinIsland$Cyclone, JoinIsland$LogArea, pch=19, cex=0.5)
box(bty='L')
axis(2)
#axis(1)
axis(1, at=1:2, labels=c('Cyclone','Not cyclone'))
mtext(paste0(LETTERS[3],'. Cyclone season'), side=3, line=0, adj=0.05)

mtext('Adjusted island area (km\u00B2)', side=2, line=2.5, outer=TRUE, las=0)
mtext(paste0(ISLAND.NAME,' Island'), adj = 0.75, side=3, line=0, cex=1.2, outer=TRUE) # margin text
dev.off()


lm.LASLP.Residuals <- residuals(lm.LASLP)
#plot(lm.LASLP)
plot(JoinIsland$time, lm.LASLP.Residuals) #residuals over time
lm.RaineTideRes <- lm(lm.LASLP.Residuals ~ JoinIsland$time)
summary(lm.RaineTideRes)
abline(lm.RaineTideRes)


###	MODEL SELECTION -
##########################################################################################

lm.Model.LASLP <- lm(ResidualArea ~ SLP_m + time + DeltaArea + DeltaTime, data = JoinIsland) #+ DeltaSLP
summary(lm.Model.LASLP)

step(lm.Model.LASLP, direction = 'backward')
#step(lm.RaineTide2, direction = 'forward') #model may need redirection, respecification

lm.Model.DASLP <- lm(DeltaArea ~ SLP_m + time + DeltaTime, data = JoinIsland) #+ DeltaSLP
summary(lm.Model.DASLP)




#log area based on sea level
#Delta area, Delta time; sea level height.
#interpretation - July to September are most significant (though January is recognised as 0 categorical, need to convert into seasons)
# if splitting up it can be perciveded that Feb to April have greatest decrease, July to September greatest increase (Feb-April may become sig if converted toseasons)
lm.RaineMaster <- lm(ResidualArea ~ DeltaArea + DeltaTime + Month, data = JoinIsland)
summary(lm.RaineMaster)

#splitting into MONTH
lm.RaineMaster <- lm(ResidualArea ~ DeltaTime + Month, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(ResidualArea ~ Month, data = JoinIsland)
summary(lm.RaineMaster)

#splitting into seasons
lm.RaineMaster <- lm(ResidualArea ~ DeltaTime + Season, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(ResidualArea ~ Season, data = JoinIsland)
summary(lm.RaineMaster)

#cyclone season
lm.RaineMaster <- lm(ResidualArea ~ DeltaTime + Cyclone, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(ResidualArea ~ Cyclone, data = JoinIsland)
summary(lm.RaineMaster)

#SOI

plot(ResidualArea ~ SOI, data = JoinIsland, ann=FALSE, axes=FALSE)
points(JoinIsland$Season, JoinIsland$LogArea, pch=19, cex=0.5)
box(bty='L')
axis(2)
#axis(1)
axis(1, at=1:3, labels=unique(JoinIsland$Event))
mtext(paste0(LETTERS[2],'. SOI'), side=3, line=0, adj=0.05)

lm.RaineMaster <- lm(ResidualArea ~ DeltaTime + SOI, data = JoinIsland)
summary(lm.RaineMaster)

lm.RaineMaster <- lm(ResidualArea ~ SOI, data = JoinIsland)
summary(lm.RaineMaster)

#JoinIsland$idoy <- as.integer(format(JoinIsland$rDate, format = '%j'))
#plot(LogArea ~ idoy, data = JoinIsland)
#loess_Island <- loess(LogArea ~ idoy, data = JoinIsland)
#plot(x=1:loess_Island$n,y=loess_Island$fitted, type='l') # come back, plot line, may need predict






########################## Plotting area changes with time ###############################
plot(JoinIsland$time, JoinIsland$AreaTime, type = 'b')
abline(h = 0)
lm.Raine <- lm(AreaTime~time, data = JoinIsland)
abline(lm.Raine, lty=2, col='forestgreen')
tsPredPlot(lm.Raine,'prediction',0.96,TRUE,2,'blue',TRUE, rgb(1,0,1,0.3))
tsPredPlot(lm.Raine,'confidence',0.96,TRUE,2,'red')


########################## Plotting tidal changes with time ###############################
#plotting Raine sea level predictions and date by line graph
plot(SLP_m ~ time, data = JoinIsland, type = 'b')
lm.Raine <- lm(SLP_m ~ time, data = JoinIsland)
abline(h=mean(JoinIsland$SLP_m))
abline(lm.Raine, lty=2, col='forestgreen')
tsPredPlot(lm.Raine,'prediction',0.96,TRUE,2,'blue',TRUE, rgb(1,0,1,0.3))
tsPredPlot(lm.Raine,'confidence',0.96,TRUE,2,'red')


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


##plotting Raine area, then also plotting tidal info
plot(JoinIsland$time, JoinIsland$DeltaArea, type = 'l')
#spread expected normal linear is 0, no change
abline(h = 0)
#linear model, change in area by time
lm.Raine <- lm(DeltaArea~time, data = JoinIsland)
abline(lm.Raine, col = 'red')
lines(JoinIsland$time, JoinIsland$HeightTime, type = 'l', col = 'blue')
#above doesn't work, two different axis ranges --> convert to proportion


##stacking plots



par(mfrow=c(1,1))

#help Matt - trying to plot the ENSO data behind the time series plots. Would be incorpated into Figure 10

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

#library(plotrix)
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
