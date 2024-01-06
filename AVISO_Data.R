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

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
#library(rgdal) # package for geospatial analysis <- this package retired, downloaded PROJ instead
library(ggplot2) # package for plotting
library(ncdf4)
library(terra)
library(PROJ)
library(reproj)
library(proj4)
library(sf)

setwd('C:/Users/alexb/OneDrive - Macquarie University/MRES - Alex Borisut/R/Aviso Data/')

#Reading in the AVISO Gridded Map, global trends
Raw_Data_Map <- nc_open('MSL_Map_MERGED_Global_AVISO_NoGIA_Adjust.nc')

#Reading in the AVISO Time series for the southern Hemisphere, from 1992
Raw_Data <- nc_open('MSL_Serie_MERGED_South_AVISO_NoGIA_1992.nc')

#Reading in the AVISO Time Series for the Southern Hemisphere, from 2012
Raw_Data <- nc_open('MSL_Serie_MERGED_South_AVISO_NoGIA_Adjust_Filter_2012.nc')

#printing metadata to text file
{sink('MSL_Serie_MERGED_South_AVISO_NoGIA_Adjust_Filter_2012_metadata.txt')
  print(Raw_Data)
  sink()
  }

#mydata.b<- brick(Raw_Data) 

#r <- raster(Raw_Data)

#a<-raster::raster('MSL_Serie_MERGED_South_AVISO_NoGIA_Adjust_Filter_2012.nc')

#msl format, m
msl.array <- ncvar_get(Raw_Data, 'msl')
head(msl)
cycle <- ncvar_get(Raw_Data, 'cycle')
head(cycle)
pass <- ncvar_get(Raw_Data, 'pass')
head(pass)

#time format, days since 1950-01-01
time <- ncvar_get(Raw_Data, 'time')
head(time)
Date <- as.Date(time, origin = '1950-01-01')
head(Date)

#determining what the fill value is for msl, fill value is 1.844674e+19
fillvalue <- ncatt_get(Raw_Data, 'msl', '_FillValue')
fillvalue

#removing fill values
msl.array[msl.array == fillvalue$value] <- NA

msl.slice <- msl.array[ , 1]


#################Raw Data Map
#printing metadata to text file
{sink('MSL_Map_MERGED_Global_AVISO_NoGIA_Adjust_metadata.txt')
  print(Raw_Data_Map)
  sink()
}

lon <- ncvar_get(Raw_Data_Map, 'longitude')
head(lon)
lat <- ncvar_get(Raw_Data_Map, 'latitude')
head(lat)
#lea level trend, expecting array of 1440 x 720
slt.array <- ncvar_get(Raw_Data_Map, 'sea_level_trends')
dim(slt.array)

#finding fill value, fill value is 1.844674e+19
fillvalue <- ncatt_get(Raw_Data_Map, 'sea_level_trends', '_FillValue')
fillvalue

slt.slice <- slt.array[, ]

#global view
r <- raster(t(slt.slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
r <- flip(r, direction = 'y')
plot(r)

#view across the GBR
#Australia <- read_sf('AUS_2021_AUST_GDA2020.shp')
#lon_gbr <- lon >= 142.350

r_GBR <- raster(xmn = 142.350, xmx = 158.024, ymn = -28.085, ymx = -9.147) #, nrow = 100, ncol = 100)
r.GBR <- crop(r, extent(r_GBR))
plot(r.GBR, legend = FALSE,  xlab = 'lon', ylab = 'lat', main = 'Great Barrier Reef: MSL - Jan 1993 to Aug 2022; Gridded Regional')
r.range <- c(minValue(r), maxValue(r))
plot(r.GBR, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 1.5, cex = 0.8))
print(r.GBR)

r_AU <- raster(xmn = 110, xmx = 165, ymn = -50, ymx = 0) #, nrow = 100, ncol = 100)

#Figure 14A
#plotting to Australia, visualisation
r.AU <- crop(r, extent(r_AU))
plot(r.AU, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Australia: MSL - Jan 1993 to Aug 2022; Gridded Regional')
r.range <- c(minValue(r), maxValue(r))
plot(r.AU, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 1.5, cex = 0.8))
mtext(paste('(A)'), adj = -0.01, side=3, line=0.9)

#pulling Raine information
r_Raine <- raster(xmn = 144.03005344188696, xmx = 144.04069644725806, ymn = -11.594353631922099, ymx = -11.587248839152902)
r.Raine <- crop(r, extent(r_Raine))
print(r.Raine)

#pulling Green information
r_Green <- raster(xmn = 145.9693489907074, xmx = 145.9784255860138, ymn = -16.76175435986674, ymx = -16.756699980862724)
r.Green <- crop(r, extent(r_Green))
print(r.Green)

#pulling Northwest information
r_Northwest <- raster(xmn = 151.69211725384523, xmx = 151.72284464031983, ymn = -23.305849722196054, ymx = -23.28803353269787)
r.Northwest <- crop(r, extent(r_Northwest))
print(r.Northwest)

#Beesley
r_Beesley <- raster(xmn = 143.1984264351654, xmx = 143.20394105690002, ymn = -12.242174209400886, ymx = -12.239406191019402)
r.Beesley <- crop(r, extent(r_Beesley))
print(r.Beesley)

#Fife
r_Fife <- raster(xmn = 143.71491194044492, xmx = 143.72355938230893, ymn = -13.656190372952988, ymx = -13.651165213434346)
r.Fife <- crop(r, extent(r_Fife))
print(r.Fife)

#Eagle
r_Eagle <- raster(xmn = 145.3723587856751, xmx = 145.38308762173466, ymn = -14.700187034180406, ymx = -14.693379190834188)
r.Eagle <- crop(r, extent(r_Eagle))
print(r.Eagle)

#Bushy Redbill
r_Bushy <- raster(xmn = 150.06618459533814, xmx = 150.07861771589708, ymn = -20.957325240280753, ymx = -20.950156269611487)
r.Bushy <- crop(r, extent(r_Bushy))
print(r.Bushy)

#Bacchi
r_Bacchi <- raster(xmn = 152.37627056610108, xmx = 152.3866131640625, ymn = -21.638025108264404, ymx = -21.632280663475065)
r.Bacchi <- crop(r, extent(r_Bacchi))
print(r.Bacchi)

#Bylund
r_Bylund <- raster(xmn = 152.4107149404449, xmx = 152.4196842473907, ymn = -21.79208738951528, ymx = -21.786508492408757)
r.Bylund <- crop(r, extent(r_Bylund))
print(r.Bylund)

#Bell
r_Bell <- raster(xmn = 151.24591106082153, xmx = 151.2529277196045, ymn = -21.81156354642245, ymx = -21.808395987194512)
r.Bell <- crop(r, extent(r_Bell))
print(r.Bell)

#Heron
r_Heron <- raster(xmn = 151.90948507270815, xmx = 151.92083618125918, ymn = -23.445310780143142, ymx = -23.439660739342145)
r.Heron <- crop(r, extent(r_Heron))
print(r.Heron)

#Erskine
r_Erskine <- raster(xmn = 151.7686907951975, xmx = 151.77489206243993, ymn = -23.504539063109604, ymx = -23.500918390805484)
r.Erskine <- crop(r, extent(r_Erskine))
print(r.Erskine)


#Aviso Jason-3 map netCDF file. https://www.aviso.altimetry.fr/en/data/products/ocean-indicators-products/mean-sea-level/data-acces.html

Raw_Data <- nc_open('MSL_Map_J3_Global_AVISO_NoGIA_Adjust.nc')

#printing metadata to text file
{sink('MSL_Map_J3_Global_AVISO_NoGIA_Adjust_metadata.nc')
  print(Raw_Data)
  sink()
}

lon <- ncvar_get(Raw_Data, 'longitude')
head(lon)
lat <- ncvar_get(Raw_Data, 'latitude')
head(lat)
#lea level trend, expecting array of 120 x 180
slt.array <- ncvar_get(Raw_Data, 'sea_level_trends')
dim(slt.array)

slt.slice <- slt.array[, ]

#global view
r <- raster(t(slt.slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
r <- flip(r, direction = 'y')
plot(r)

#GBR view
r_GBR <- raster(xmn = 142.350, xmx = 158.024, ymn = -28.085, ymx = -9.147) #, nrow = 100, ncol = 100)
r.GBR <- crop(r, extent(r_GBR))
plot(r.GBR, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Great Barrier Reef: MSL - Feb 2016 to Aug 2023; Jason-3')
r.range <- c(minValue(r), maxValue(r))
plot(r.GBR, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))
print(r.GBR)

#Figure 14B
#plotting to Australia, visualisation
r.AU <- crop(r, extent(r_AU))
plot(r.AU, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Australia: MSL - Feb 2016 to Aug 2023; Jason-3')
r.range <- c(minValue(r), maxValue(r))
plot(r.AU, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))
mtext(paste('(B)'), adj = -0.01, side=3, line=0.9)

r.Raine <- crop(r, extent(r_Raine))
print(r.Raine)

#pulling Green information
r.Green <- crop(r, extent(r_Green))
print(r.Green)

#pulling Northwest information
r.Northwest <- crop(r, extent(r_Northwest))
print(r.Northwest)

#Beesley
r.Beesley <- crop(r, extent(r_Beesley))
print(r.Beesley)

#Fife
r.Fife <- crop(r, extent(r_Fife))
print(r.Fife)

#Eagle
r.Eagle <- crop(r, extent(r_Eagle))
print(r.Eagle)

#Bushy Redbill
r.Bushy <- crop(r, extent(r_Bushy))
print(r.Bushy)

#Bacchi
r.Bacchi <- crop(r, extent(r_Bacchi))
print(r.Bacchi)

#Bylund
r.Bylund <- crop(r, extent(r_Bylund))
print(r.Bylund)

#Bell
r.Bell <- crop(r, extent(r_Bell))
print(r.Bell)

#Heron
r.Heron <- crop(r, extent(r_Heron))
print(r.Heron)

#Erskine
r.Erskine <- crop(r, extent(r_Erskine))
print(r.Erskine)

#######################################
##AVISO sentinel-6 sea level trends https://www.aviso.altimetry.fr/en/data/products/ocean-indicators-products/mean-sea-level/data-acces.html
#period 2021 to 2023

Raw_Data <- nc_open('MSL_Map_S6A_Global_AVISO_NoGIA_Adjust.nc')

#printing metadata to text file
{sink('MSL_Map_S6A_Global_AVISO_NoGIA_Adjust_metadata.nc')
  print(Raw_Data)
  sink()
}

lon <- ncvar_get(Raw_Data, 'longitude')
head(lon)
lat <- ncvar_get(Raw_Data, 'latitude')
head(lat)
#lea level trend, expecting array of 120 x 180
slt.array <- ncvar_get(Raw_Data, 'sea_level_trends')
dim(slt.array)

slt.slice <- slt.array[, ]

#global view
r <- raster(t(slt.slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
r <- flip(r, direction = 'y')
plot(r)

#GBR view
r_GBR <- raster(xmn = 142.350, xmx = 158.024, ymn = -28.085, ymx = -9.147) #, nrow = 100, ncol = 100)
r.GBR <- crop(r, extent(r_GBR))
plot(r.GBR, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Great Barrier Reef: MSL - Sept 2021 to Oct 2023; Sentinel-6')
r.range <- c(minValue(r), maxValue(r))
plot(r.GBR, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))
print(r.GBR)

#Not included in Figure
#plotting to Australia, visualisation
r.AU <- crop(r, extent(r_AU))
plot(r.AU, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Australia: MSL - Sept 2021 to Oct 2023; Sentinel-6')
r.range <- c(minValue(r), maxValue(r))
plot(r.AU, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))

r.Raine <- crop(r, extent(r_Raine))
print(r.Raine)

#pulling Green information
r.Green <- crop(r, extent(r_Green))
print(r.Green)

#pulling Northwest information
r.Northwest <- crop(r, extent(r_Northwest))
print(r.Northwest)

#Beesley
r.Beesley <- crop(r, extent(r_Beesley))
print(r.Beesley)

#Fife
r.Fife <- crop(r, extent(r_Fife))
print(r.Fife)

#Eagle
r.Eagle <- crop(r, extent(r_Eagle))
print(r.Eagle)

#Bushy Redbill
r.Bushy <- crop(r, extent(r_Bushy))
print(r.Bushy)

#Bacchi
r.Bacchi <- crop(r, extent(r_Bacchi))
print(r.Bacchi)

#Bylund
r.Bylund <- crop(r, extent(r_Bylund))
print(r.Bylund)

#Bell
r.Bell <- crop(r, extent(r_Bell))
print(r.Bell)

#Heron
r.Heron <- crop(r, extent(r_Heron))
print(r.Heron)

#Erskine
r.Erskine <- crop(r, extent(r_Erskine))
print(r.Erskine)


#####################################################################
## Saral/Altika sea level trends - AVISO: https://www.aviso.altimetry.fr/en/data/products/ocean-indicators-products/mean-sea-level/data-acces.html

Raw_Data <- nc_open('MSL_Map_AL_Global_AVISO_NoGIA_Adjust.nc')

#printing metadata to text file
{sink('MSL_Map_AL_Global_AVISO_NoGIA_Adjust_metadata.nc')
  print(Raw_Data)
  sink()
}

lon <- ncvar_get(Raw_Data, 'longitude')
head(lon)
lat <- ncvar_get(Raw_Data, 'latitude')
head(lat)
#lea level trend, expecting array of 120 x 180
slt.array <- ncvar_get(Raw_Data, 'sea_level_trends')
dim(slt.array)

slt.slice <- slt.array[, ]

#global view
r <- raster(t(slt.slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
r <- flip(r, direction = 'y')
plot(r)

#GBR view
r_GBR <- raster(xmn = 142.350, xmx = 158.024, ymn = -28.085, ymx = -9.147) #, nrow = 100, ncol = 100)
r.GBR <- crop(r, extent(r_GBR))
plot(r.GBR, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Great Barrier Reef: MSL - Mar 2013 to Sept 2023; Saral/Altika')
r.range <- c(minValue(r), maxValue(r))
plot(r.GBR, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))
print(r.GBR)

#Figure 14C
#plotting to Australia, visualisation
r.AU <- crop(r, extent(r_AU))
plot(r.AU, legend = FALSE, xlab = 'lon', ylab = 'lat', main = 'Australia: MSL - Mar 2013 to Sept 2023; Saral/Altika')
r.range <- c(minValue(r), maxValue(r))
plot(r.AU, legend.only = TRUE,
     legend.width = 1, legend.shrink = 0.75,
     legend.args=list(text='mm/Year', side = 4, font = 2, line = 2, cex = 0.8))
mtext(paste('(C)'), adj = -0.01, side=3, line=0.9)

r.Raine <- crop(r, extent(r_Raine))
print(r.Raine)

#pulling Green information
r.Green <- crop(r, extent(r_Green))
print(r.Green)

#pulling Northwest information
r.Northwest <- crop(r, extent(r_Northwest))
print(r.Northwest)

#Beesley
r.Beesley <- crop(r, extent(r_Beesley))
print(r.Beesley)

#Fife
r.Fife <- crop(r, extent(r_Fife))
print(r.Fife)

#Eagle
r.Eagle <- crop(r, extent(r_Eagle))
print(r.Eagle)

#Bushy Redbill
r.Bushy <- crop(r, extent(r_Bushy))
print(r.Bushy)

#Bacchi
r.Bacchi <- crop(r, extent(r_Bacchi))
print(r.Bacchi)

#Bylund
r.Bylund <- crop(r, extent(r_Bylund))
print(r.Bylund)

#Bell
r.Bell <- crop(r, extent(r_Bell))
print(r.Bell)

#Heron
r.Heron <- crop(r, extent(r_Heron))
print(r.Heron)

#Erskine
r.Erskine <- crop(r, extent(r_Erskine))
print(r.Erskine)


