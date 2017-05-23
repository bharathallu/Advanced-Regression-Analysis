bridge<-read.csv("/Users/home/Documents/TAMU/stat608/sp13/Notes/bridge.csv", header=TRUE)

attach(bridge)

#Step 1:  Explore the data.
plot(bridge)

#Looks like everything is skewed.
hist(Time)
hist(DArea)
hist(CCost)
hist(Dwgs)
hist(Length)
hist(Spans)

#Notice Spans is discrete, with not too many distinct values:
table(Spans)



#Step 2:  Transform it all.  First the X's:
X<-cbind(DArea,CCost,Dwgs,Length,Spans)
library(car)
tranx<-powerTransform(X)
summary(tranx)

hist(Length)
hist(log(Length))

#Then the response variable:

lm1<-lm(Time ~ log(DArea) + log(CCost)+log(Dwgs)+log(Length)+log(Spans))
trany<-powerTransform(lm1)
summary(trany)

#Step 3:  Fit our transformed model, and see whether our model is valid:
lm.logs<-lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
pairs(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lm.logs)

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(3,2))
plot(lm.logs$fitted, lm.logs$residuals)
plot(log(DArea), lm.logs$residuals)
plot(log(CCost), lm.logs$residuals)
plot(log(Dwgs), lm.logs$residuals)
plot(log(Length), lm.logs$residuals)
plot(log(Spans), lm.logs$residuals)

par(cex.axis=2,cex.lab=1.5, mar=c(5.2,5.2,2,2),lwd=2, pch=19, mfcol=c(3,2))
library(alr3)
mmps(lm.logs)

#Looks good; let's check out the regression output:
summary(lm.logs)
#Notice log(DArea) and log(Length) have the wrong sign!

#Look at the correlation between the predictor variables:
X<-cbind(log(DArea), log(CCost), log(Dwgs), log(Length), log(Spans))
cor(X)
#Remember this correlation matrix won't pick up on everything, e.g. x2 = x1 + x5.

#Added variable plots:
library(car)
par(cex.axis=1.5,cex.lab=1.5, mar=c(6,7,4,4),lwd=2, pch=19, mfcol=c(3,2))
avPlots(lm.logs)

#Variance Inflation Factors:
vif(lm.logs)
#Several larger than 5.  Use methods in Chapter 7.











