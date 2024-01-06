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

library(dplyr)

##Reading data file
#Raw_Data <- read.csv('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/Data/R/All_Cays_RAW.csv')
setwd('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/Data/R/')
Raw_Data <- read.csv('All_Cays_RAW.csv')


#square root of area by pi
Raw_Data$Rarea <- sqrt(Raw_Data$Area.km2/pi)
#perimeter by pi
Raw_Data$Rperim <- Raw_Data$Perimeter.km/(2*pi)
#plotting area to investigate 
plot(Rperim~Rarea,data = Raw_Data)
#adding a line for the expected normality line - looking at spread
abline(a=0, b=1)
#summarising raw data to see spread and differences etc
summary(Raw_Data)
unique(Raw_Data$Water.Index)

#index prefs = the preferred plotting of symbol and colour per water index
index_prefs <- data.frame(Water.Index = unique(Raw_Data$Water.Index), pch = c(21,23, 24), col = c('blue','red','green'))
index_prefs
data2 <- merge(Raw_Data, index_prefs)
head(data2)


plotUncert  <- function(model, interval='prediction', level=0.95, plotLines=TRUE, lty=2, lineCol='black', plotArea=FALSE, areaColor) {
	
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

# FIGURE 6
######################################################
pdf(file=paste0('../Output/Fig6-RadiusCompared.pdf'))
par(oma=c(0,0,0,0), mar=c(4,4,1,1))


plot(Rperim~Rarea,data = data2, pch = data2$pch, bg = data2$col, xlab = 'Area Radius (km)', ylab = 'Perimeter Radius (km)')
abline(a=0, b=1)

##looking at perimeter and area correlation/association/relationship
#linear model of NDWI, where perimeter is plotted against area for NDWI
lm.NDWI <- lm(Rperim~Rarea,data = data2[(data2$Water.Index=='NDWI'), ])
#adding NDWI linear model line for NDWI
abline(lm.NDWI, col = 'blue')
plotUncert(lm.NDWI, plotLine=FALSE, lineCol='blue',plotArea=TRUE, areaColor=rgb(0,0,1,0.1))
#linear model of MNDWI, where perimeter is plotted against area for MNDWI
lm.MNDWI <- lm(Rperim~Rarea,data = data2[(data2$Water.Index=='MNDWI'), ])
#adding MNDWI linear model line for MNDWI
abline(lm.MNDWI, col = 'red')
plotUncert(lm.MNDWI, plotLine=FALSE, lineCol='red',plotArea=TRUE, areaColor=rgb(1,0,0,0.1))
#linear model for SWI, where perimeter is plotted against area for SWI
lm.SWI <- lm(Rperim~Rarea,data = data2[(data2$Water.Index=='SWI'), ])
#addding SWI linear model line for SWI
abline(lm.SWI, col = 'green')
plotUncert(lm.SWI, plotLine=FALSE, lineCol='green',plotArea=TRUE, areaColor=rgb(0,1,0,0.1))
#summary statistics of the linear models
summary(lm.SWI)
summary(lm.MNDWI)
summary(lm.NDWI)

dev.off()


plot(Area.km2~Index.Cutoff,data = data2, pch = data2$pch, bg = data2$col)

#for loops for the first and last area for years etc
plot(Area.km2~Year,data = data2, pch = data2$pch, bg = data2$col, xlim = c(2015, 2026))
cays <- unique(data2$Island)
for(c in cays) {
  for(i in index_prefs$Water.Index) {
  temp <- data2[(data2$Island==c & data2$Water.Index==i),]
  segments(temp$Year[1], temp$Area.km2[1], temp$Year[2], temp$Area.km2[2], col = temp$col,)
  text(temp$Year[2], temp$Area.km2[2], temp$Island[2], cex = 0.6, pos = 4,)
  }}

#for loops for the first and last area for years etc - attempt with log scale
plot(log(Area.km2)~Year,data = data2, pch = data2$pch, bg = data2$col, xlim = c(2015, 2026), main = 'Log')
cays <- unique(data2$Island)
for(c in cays) {
  for(i in index_prefs$Water.Index) {
    temp <- data2[(data2$Island==c & data2$Water.Index==i),]
    segments(temp$Year[1], log(temp$Area.km2[1]), temp$Year[2], log(temp$Area.km2[2]), col = temp$col,)
    text(temp$Year[2], log(temp$Area.km2[2]), temp$Island[2], cex = 0.6, pos = 4,)
  }}

# FIGURE 7
######################################################
pdf(file=paste0('../Output/Fig7-FirstLastYears.pdf'))
par(oma=c(0,0,0,0), mar=c(4,4,1,1), pin=c(3,6))

#for loops for the first and last area for years etc - attempt with log 10 scale
plot(log10(Area.km2)~Year,data = data2, pch = data2$pch, bg = data2$col, xlim = c(2015, 2025.5), ann = FALSE, las=1)
mtext('Log10 Island Area (km\u00B2)',side=2, line=3, las=0)
cays <- unique(data2$Island)
for(c in cays) {
  for(i in index_prefs$Water.Index) {
    temp <- data2[(data2$Island==c & data2$Water.Index==i),]
    segments(temp$Year[1], log10(temp$Area.km2[1]), temp$Year[2], log10(temp$Area.km2[2]), col = temp$col,)
    text(temp$Year[2], log10(temp$Area.km2[2]), temp$Island[2], cex = 0.6, pos = 4,)
  }}

##mean
mean_area <- aggregate(data2$Area.km2, by = list(data2$Island, data2$Year), FUN = mean)
data2$area_diff <- NA

dev.off()

#plotting differences from mean
for (i in 1:nrow(mean_area)) {
  data2[(data2$Island==mean_area[i, 'Group.1']) & (data2$Year==mean_area[i, 'Group.2']), 'area_diff'] <- mean_area[i, 'x'] - data2[(data2$Island==mean_area[i, 'Group.1']) & (data2$Year==mean_area[i, 'Group.2']), 'Area.km2']
}

plot(data2$area_diff)

#FIGURE 5
######################################################
pdf(file=paste0('../Output/Fig5-IndexDiff.pdf'), paper='A4')
par(oma=c(1,1,0,0), mar=c(4,4,1,1), pin=c(3,6))

plot(area_diff~ as.factor(Water.Index), data = data2, xlab = 'Index', ylab = 'Proportion Difference', las=1)
abline(h=0)
points(as.factor(data2$Water.Index), data2$area_diff, pch= data2$pch, bg= data2$col)

#difference of standard deviation
diff_sd <- aggregate(data2$area_diff, by = list(data2$Water.Index), FUN = sd)
diff_sd

#difference of interquartile range
diff_iqr <- aggregate(data2$area_diff, by = list(data2$Water.Index), FUN = IQR)
diff_iqr

#difference means
diff_mean <- aggregate(data2$area_diff, by = list(data2$Water.Index), FUN = mean)
diff_mean

#significance test for the difference in area between indices --> note the difference is significant due to MNDWI being the most different.
kruskal.test(data2$area_diff, data2$Water.Index)

wilcox.test(data2[(data2$Water.Index=='MNDWI'), 'area_diff'])
wilcox.test(data2[(data2$Water.Index=='NDWI'), 'area_diff'])
wilcox.test(data2[(data2$Water.Index=='SWI'), 'area_diff'])

dev.off()

#next steps
# look at area/perimeter by laltitude
#perform kruskal tests on the differences between indexes individually. 

#grouped_wilcox(
#  data = data2,
#  dep.vars = area_diff,
#  indep.vars = Water.Index,
#  grouping.vars = Water.Index,
#  paired = FALSE,
#  correct = TRUE
#)

wilcox.test(data2$Water.Index, data2$area_diff)

data2 %>%
  group_by(Water.Index) %>%
  summarise(p_value = wilcox.test(Data = data2, area_diff, exact = FALSE)$p.value)

wilcox_indices <- pairwise.wilcox.test(data2$area_diff, data2$Water.Index, p.adjust.method="none")
wilcox_indices

















ISLAND.NAME <- 'Green'
#ISLAND.NAME <- 'Raine'
#ISLAND.NAME <- 'North West'

if (ISLAND.NAME == 'Green') {
	ISLAND.FILE <- 'analysisData-Green.csv'
}
if (ISLAND.NAME == 'Raine') {
	ISLAND.FILE <- 'analysisData-Raine.csv'
}
if (ISLAND.NAME == 'North West') {
	ISLAND.FILE <- 'analysisData-Northwest.csv'
}

JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)


###	OPEN PLOT - LOG AREA AS PREDICTED BY SEA LEVEL
##########################################################################################
pdf(file=paste0('../Output/Fig9-All-LASLP-LOG.pdf'), width=3.5, height=8)
par(oma=c(3,4,1,1), mar=c(1,1,0,0), mfcol=c(3,1))

i <- 1

for (ISLAND.NAME in c('Raine','Green','North West')) {

	ISLAND.FILE <- paste0('analysisData-', ISLAND.NAME,'.csv')
	
	JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)

	plot(JoinIsland$SLP_m, JoinIsland$LogArea, ann=FALSE, axes=FALSE, type='n', xlim=c(0.5,4))
	box(bty='L')
	axis(2, las=1)
	lm.LASLP <- lm(LogArea ~ SLP_m, data = JoinIsland)
	
	plotUncert(lm.LASLP, interval='prediction', plotLines=TRUE, lty=2, lineCol='navy', plotArea=TRUE, areaColor=rgb(0,1,1,0.5))
	plotUncert(lm.LASLP, interval='confidence', plotLines=TRUE, lty=2, lineCol='navy', plotArea= TRUE, areaColor=rgb(0,1,1,0.5))
	abline(lm.LASLP, lwd=2, lty=1, col='navy')
	points(JoinIsland$SLP_m, JoinIsland$LogArea, pch=19, cex=0.5, col='navy')
	
	rsqr <- round(summary(lm.LASLP)$adj.r.squared,3) #pulling adjusted r2 from summary of lm
	mtext(paste0(ISLAND.NAME,' Island'), adj = 0.95, side=3, line=-2.5, cex=1.2) # margin text print r2
	mtext(paste('r\u00b2 =', rsqr), adj = 0.95, side=3, line=-4) # margin text print r2
	mtext(paste0(LETTERS[i],'.'), adj = 0.05, side=1, line=-1.5, cex=1.2) # margin text print r2
	i <- i + 1
}

mtext('Tide Height (m)', side=1, line=1.7, las=0, outer=TRUE)
mtext('Log10 Island Area (m\u00B2)', side=2, line=2.5, las=0, outer=TRUE)
axis(1)

dev.off()



pdf(file=paste0('../Output/Fig9-All-LASLP-areth.pdf'), width=3.5, height=8)
par(oma=c(3,4,1,1), mar=c(1,1,0,0), mfcol=c(3,1))

i <- 1

for (ISLAND.NAME in c('Raine','Green','North West')) {

	ISLAND.FILE <- paste0('analysisData-', ISLAND.NAME,'.csv')
	
	JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)

	plot(JoinIsland$SLP_m, JoinIsland$Area..km2., ann=FALSE, axes=FALSE, type='n', xlim=c(0.5,4))
	box(bty='L')
	axis(2, las=1)
	lm.LASLP <- lm(Area..km2. ~ SLP_m, data = JoinIsland)
	
	plotUncert(lm.LASLP, interval='prediction', plotLines=TRUE, lty=2, lineCol='navy', plotArea=TRUE, areaColor=rgb(0,1,1,0.5))
	plotUncert(lm.LASLP, interval='confidence', plotLines=TRUE, lty=2, lineCol='navy', plotArea= TRUE, areaColor=rgb(0,1,1,0.5))
	abline(lm.LASLP, lwd=2, lty=1, col='navy')
	points(JoinIsland$SLP_m, JoinIsland$Area..km2., pch=19, cex=0.5, col='navy')
	
	rsqr <- round(summary(lm.LASLP)$adj.r.squared,3) #pulling adjusted r2 from summary of lm
	mtext(paste0(ISLAND.NAME,' Island'), adj = 0.95, side=3, line=-2.5, cex=1.2) # margin text print r2
	mtext(paste('r\u00b2 =', rsqr), adj = 0.95, side=3, line=-5.5, cex=0.8) # margin text print r2
	mtext(paste0('Area = ',round(coefficients(lm.LASLP)[2],2),'\u00B7Tide + ',round(coefficients(lm.LASLP)[1],1)), adj = 0.95, side=3, line=-4, cex=0.8) # margin text print r2

	mtext(paste0(LETTERS[i],'.'), adj = 0.05, side=1, line=-1.5, cex=1.2) # margin text print r2
	i <- i + 1
}

mtext('Tide Height (m)', side=1, line=1.7, las=0, outer=TRUE)
mtext('Island Area (km\u00B2)', side=2, line=2.5, las=0, outer=TRUE)
axis(1)

dev.off()



pdf(file=paste0('../Output/Fig9-All-TideResiduals-areth.pdf'), width=6, height=8)
par(oma=c(3,4,1,1), mar=c(1,2,2,0), mfcol=c(3,1))

i <- 1

for (ISLAND.NAME in c('Raine','Green','North West')) {

	ISLAND.FILE <- paste0('analysisData-', ISLAND.NAME,'.csv')
	
	JoinIsland <- read.csv(file = ISLAND.FILE, row.names=1)


	#Area tide residuals
	#######################################################
	lm.TideArea <- lm(Area..m2. ~ SLP_m, data = JoinIsland)
	tideAreaResiduals <- residuals(lm.TideArea)
	lm.TideResiduals <- lm(tideAreaResiduals ~ time, data = JoinIsland)
	plot(tideAreaResiduals ~ time, data=JoinIsland, type = 'b', axes=FALSE, ann=FALSE, bty='L', las=1)
	axis(2, las=1)
	box(bty='L')
	abline(h=0)

abline(lm.TideResiduals, col = 'red')

	fStat <- summary(lm.TideResiduals)$fstatistic
	pVal <- 1 - round(pf(q=fStat[1], df1=fStat[2], df2=fStat[3]),3)
	pValText <- paste('p =', pVal)
	
	if (pValText  == 'p = 0') pValText <- 'p < 0.001'

#pVal <- tsPlotStats(lm.TideResiduals, r2=FALSE, pv=TRUE)
if (pVal < 0.05) {plotArea <- TRUE} else {plotArea <- FALSE}

tsPredPlot(lm.TideResiduals,'prediction',0.96,TRUE,2,'blue',plotArea, rgb(1,0,1,0.3))
tsPredPlot(lm.TideResiduals,'confidence',0.96,TRUE,2,'red')
abline(h=mean(tideAreaResiduals), lty=1, col='forestgreen')


	rsqr <- round(summary(lm.TideResiduals)$adj.r.squared,3) #pulling adjusted r2 from summary of lm
	mtext(paste('r\u00b2 =', rsqr), adj = 0.95, side=3, line=-1.5, cex=0.8) # margin text print r2

	mtext(paste0('Area = ',round(coefficients(lm.TideResiduals)[2],0),'\u00B7TideResidual + ',round(coefficients(lm.TideResiduals)[1],0)), adj = 0.95, side=3, line=0, cex=0.8) # margin text print r2

	mtext(pValText, adj = 0.95, side=3, line=-3, cex=0.8 )# margin text print r2


	mtext(paste0(LETTERS[i],'.'), adj = 0, side=3, line=0, cex=1.2) # margin text print r2
	mtext(paste0(ISLAND.NAME,' Island'), adj = 0.1, side=3, line=0, cex=1.2) # margin text print r2
	i <- i + 1
}

mtext('Time', side=1, line=1.7, las=0, outer=TRUE)
mtext('Island Area adjusted for tide hights (m\u00B2)', side=2, line=2.5, las=0, outer=TRUE)
axis(1)

dev.off()


