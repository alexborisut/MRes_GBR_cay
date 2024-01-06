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

#Use the sf package to import shapefiles
library(sf)

#Use the sp package to calculate distances
library(sp)


filename <- "AUS_2021_AUST_GDA2020.shp"
nc <- st_read(filename)
(nc_geom <- st_geometry(nc))

# see variables
str(nc)

# see first 3 rows
print(nc, n=3)

head(nc)

##	island locations
isl <- data.frame(Lat=1:3,Lng=1:3,name=c('Green','Raine','North West'))
isl[1,'Lat'] <- -1 * (16 + 45.5/60)
isl[1,'Lng'] <- 145 + 58.5/60
isl[2,'Lat'] <- -1 * (11 + 36/60)
isl[2,'Lng'] <- 144 + 1/60
isl[3,'Lat'] <- -1 * (23 + 18/60)
isl[3,'Lng'] <- 151 + 45/60


# READ IN CYCLONE DATA
cycloneData <- read.csv(file = 'IDCKMSTM0S_cleaned_AEST.csv')
cycloneData <- cycloneData[(cycloneData$Year > '2013'), ]
cycloneData <- cycloneData[(cycloneData$LON > 0), ]

cycloneData$distance <- spDistsN1(pts=as.matrix(cycloneData[,c('LON','LAT')]),pt=c(isl[2,'Lng'], isl[2,'Lat']),longlat=TRUE)

cyclone2 <- cycloneData[,c('DISTURBANCE_ID','ï..NAME','Date_Time_AEST','SURFACE_CODE','LAT','LON','distance','MAX_WIND_SPD','DVORAK_CI_NO')]
cyclone2$rDate <- strptime(cyclone2$Date_Time_AEST, format = '%Y/%m/%d %I:%M:%S %p')
cyclone2$SimpleDate <- as.Date(cyclone2$rDate, format = '%Y-%M-%D')
cyclone2$iYear <- as.integer(format(cyclone2$rDate, format = '%Y'))
cyclone2$idoy <- as.integer(format(cyclone2$rDate, format = '%j'))
cyclone2$time <- (cyclone2$iYear - min(cyclone2$iYear)) * 365 + cyclone2 $idoy


cyclone3 <- cyclone2 
cyclone3 <- cyclone3[(round(cyclone3$LAT)>-28) & (round(cyclone3$LAT)<-10),]
cyclone3 <- cyclone3[(round(cyclone3$LON)>142) & (round(cyclone3$LON)<155),]

pdf(file=paste0('../Output/CycloneTracks.pdf'), height=10, width=3.4, paper='A4')
par(mfrow=c(5,2), oma=c(3,3,1,1), mar=c(0.5,0.5,0,0))
# 

#pdf('~/Desktop/shape.pdf')
# plot(nc_geom)

##
##	FIX: 2020, 2022, 2023 - striaght lines
## - try making line width relative to intensity...
##



for (Y in 2014:2023) {

#plot(1:1, type='n',xlim=c(110,155),ylim=c(-45,-10))
plot(1:1, type='n',xlim=c(142,155),ylim=c(-28,-10), axes=FALSE, ann=TRUE)
for (i in 1:length(nc_geom)) {
	plot(nc_geom[i], col=rgb(0.5, 0.5, 0.5, 0.5), lwd=0.1, add=TRUE)
	box(bty='o')
}
	
	if ((Y %% 2) == 0)
		axis(2)
	if (Y > 2021)
		axis(1)
	
	# CYCLONES
#	cyclones <- unique(cyclone3[(cyclone3$iYear == Y),'NAME'])
	cyclones <- unique(cyclone3[(cyclone3$iYear == Y),'DISTURBANCE_ID'])
	cColours <- rainbow(length(cyclones))

	i <- 0
	tmpName <- vector()
	if (length(cyclones) > 0) {
	for (c in cyclones) {
		i <- i + 1
		tmp <- cycloneData[((cyclone2$DISTURBANCE_ID == c) & (cyclone2$iYear == Y)),]
		tmpName[i] <- tmp[1,'ï..NAME']
		lines(tmp$LON,tmp$LAT, col=cColours[i], lwd=1)
		
		tmp <- tmp[(tmp$DVORAK_CI_NO > 1),]
		lines(tmp$LON,tmp$LAT, col=cColours[i], lwd=3)

		tmp <- tmp[(tmp$DVORAK_CI_NO > 2),]
		lines(tmp$LON,tmp$LAT, col=cColours[i], lwd=5)

		tmp <- tmp[(tmp$DVORAK_CI_NO > 3),]
		lines(tmp$LON,tmp$LAT, col=cColours[i], lwd=7)

		tmp <- tmp[(tmp$DVORAK_CI_NO > 4),]
		lines(tmp$LON,tmp$LAT, col=cColours[i], lwd=9)
		
		
		
		## comment this out to skip the points indicating intensity
#		points(tmp$LON,tmp$LAT, col=cColours[i], pch=20, cex=(0.1+ tmp$DVORAK_CI_NO/2))
	}
	
	if (Y == '2016') {
		text(144,-21,'Category',pos=3, cex=0.8)
		legend(142,-20,legend=c('0','1','2','3','4'),col='black', lty=1, bty='n', cex=0.7, lwd=c(1,3,5,7,9))
	}
	legend('bottomleft',legend= tmpName,col=cColours, lty=1, bty='n', cex=0.7, lwd=2)
	}
	
	# ISLANDS
	points(isl$Lng,isl$Lat,pch=19, cex=0.8)
	text(isl$Lng,isl$Lat,isl$name, pos=3, cex=1)

	# YEAR
	mtext(Y, side=3, line=-1.5, adj=0.95)

}
mtext('Latitude',side=2, line=1.8, outer=TRUE)
mtext('Longitude',side=1, line=1.8, outer=TRUE)

dev.off()
