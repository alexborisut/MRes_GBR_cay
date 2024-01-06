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

#Script to be run AFTER Raine, Green and North West scripts

par(mfrow=c(3,1))

#Figure 9

####################### RAINE
#relationship between sea level and area
plot(AreaLog ~ Raine_SLP_m, data = JoinRaine, ylab = '', xlab = '')
lm.RaineTide <- lm(AreaLog ~ Raine_SLP_m, data = JoinRaine)
abline(lm.RaineTide)

pSLP <- seq(min(JoinRaine$Raine_SLP_m)-0.1,max(JoinRaine$Raine_SLP_m)+0.1,length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.RaineTide, newdata= data.frame(Raine_SLP_m = pSLP), interval='prediction', level = 0.95) #prediction using lm.Raine
polygon(c(pSLP,rev(pSLP)),c(preds[,2],rev(preds[,3])), col = 'skyblue', border=NA)
lines(pSLP, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pSLP, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points
points(JoinRaine$Raine_SLP_m, JoinRaine$AreaLog, pch = 19, cex=0.5)
abline(lm.RaineTide)

predc <- predict.lm(lm.RaineTide, newdata= data.frame(Raine_SLP_m = pSLP), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pSLP, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pSLP, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

rsqr <- round(summary(lm.RaineTide)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.9, side=3, line=-2) # margin text print r2
mtext(paste('A'), adj = -0.01, side=3, line=0.9) # margin text plot letter

############
##########
############## GREEN

#relationship between sea level and area
plot(AreaLog ~ Green_SLP_m, data = JoinGreen, ylab = 'Log10 Area (m2)', xlab = '')
lm.GreenTide <- lm(AreaLog ~ Green_SLP_m, data = JoinGreen)
abline(lm.GreenTide)

pSLP <- seq(min(JoinGreen$Green_SLP_m)-0.1,max(JoinGreen$Green_SLP_m)+0.1,length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.GreenTide, newdata= data.frame(Green_SLP_m = pSLP), interval='prediction', level = 0.95) #prediction using lm.Raine
polygon(c(pSLP,rev(pSLP)),c(preds[,2],rev(preds[,3])), col = 'skyblue', border=NA)
lines(pSLP, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pSLP, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points
points(JoinGreen$Green_SLP_m, JoinGreen$AreaLog, pch = 19, cex=0.5)
abline(lm.GreenTide)

predc <- predict.lm(lm.GreenTide, newdata= data.frame(Green_SLP_m = pSLP), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pSLP, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pSLP, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

rsqr <- round(summary(lm.GreenTide)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.9, side=3, line=-2) # margin text print r2
mtext(paste('B'), adj = -0.01, side=3, line=0.9) # margin text plot letter


############################
########################
####### North West

#relationship between sea level and area
plot(AreaLog ~ Heron_SLP_m, data = JoinNorthwest, xlab = 'Sea Level Prediction (m)', ylab = '')
lm.NorthwestTide <- lm(AreaLog ~ Heron_SLP_m, data = JoinNorthwest)
abline(lm.NorthwestTide)

pSLP <- seq(min(JoinNorthwest$Heron_SLP_m)-0.1,max(JoinNorthwest$Heron_SLP_m)+0.1,length.out=100) #predicted times - from lowest value to highest, 100 points equally spaced
preds <- predict.lm(lm.NorthwestTide, newdata= data.frame(Heron_SLP_m = pSLP), interval='prediction', level = 0.95) #prediction using lm.Raine
polygon(c(pSLP,rev(pSLP)),c(preds[,3],rev(preds[,2])), col = 'skyblue', border=NA)
lines(pSLP, preds[,'lwr'], lty = 2, col = 'blue') #lower prediction interval, outlier points
lines(pSLP, preds[,'upr'], lty = 2, col = 'blue') # upper prediction interval, outlier points
points(JoinNorthwest$Heron_SLP_m, JoinNorthwest$AreaLog, pch = 19, cex=0.5)
abline(lm.NorthwestTide)

predc <- predict.lm(lm.NorthwestTide, newdata= data.frame(Heron_SLP_m = pSLP), interval='confidence', level = 0.95) #confidence using lm.Raine
#polygon(c(pTime,rev(pTime)),c(preds[,2],rev(preds[,3])), col = NA, border=2)
lines(pSLP, predc[,'lwr'], lty = 2, col = 'red') #lower confidence interval, outlier points
lines(pSLP, predc[,'upr'], lty = 2, col = 'red') # upper confidence interval, outlier points

rsqr <- round(summary(lm.NorthwestTide)$adj.r.squared,4) #pulling adjusted r2 from summary of lm
mtext(paste('r\u00b2 =', rsqr), adj = 0.9, side=3, line=-2) # margin text print r2
mtext(paste('C'), adj = -0.01, side=3, line=0.9) # margin text plot letter


# investigating outliers
par(mfrow=c(1,1))
#Raine
cooksd <- cooks.distance(lm.RaineTide)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+3, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(JoinRaine[influential, ])  # influential observations.

#Green
cooksd <- cooks.distance(lm.GreenTide)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+3, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
print(JoinGreen[influential, ])  # influential observations.

#North West
cooksd <- cooks.distance(lm.NorthwestTide)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+3, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
print(JoinNorthwest[influential, ])  # influential observations.
