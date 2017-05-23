bonds<-read.csv("/Users/Elizabeth/Documents/TAMU/stat608/Data/bonds.csv",header=TRUE)

bonds<-read.csv("U:\\608\\sp13\\Notes\\bonds.csv", header=TRUE)


#Suggestions on examining data properties:
summary(bonds)
class(bonds)
attributes(bonds)
str(bonds)


attach(bonds)  #We don't need to use datasetname$variablename when we attach; instead, just use variable names.


my.lm<-lm(BidPrice ~ CouponRate)


#Create a plot to see what the regression line looks like:
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,3)) 
plot(CouponRate, BidPrice, xlab="Coupon Rate", ylab="Bid Price", pch=19)
#Plot the regression line on top of the points.
abline(my.lm)  
#This allows us to identify the case number of each point by clicking on the plot:
identify(CouponRate, BidPrice,Case)  

#Remember: not the way to calculate z-scores for residuals!
z.resids<-(my.lm$residuals - mean(my.lm$residuals))/sd(my.lm$residuals)

#Instead, do:
s<-summary(my.lm)$sigma  #The standard deviation of the residuals
hii<-hatvalues(my.lm)  #The leverages
z.resids<-my.lm$residuals / (s*sqrt(1 - hii))

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(bonds$CouponRate, z.resids, xlab="Coupon Rate", ylab="Standardized Residuals", pch=19)
abline(h=-2,lty=2)
abline(h=2,lty=2)
identify(CouponRate, z.resids,Case)

#Here I'm going to just drop the three outliers:
m2 <- update(my.lm, subset=(1:35)[-c(4,13,35)])
summary(m2)

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(CouponRate[-c(4,13,35)], BidPrice[-c(4,13,35)],xlab="Coupon Rate", ylab="Bid Price",ylim=c(85,120),xlim=c(2,14), pch=19)
abline(m2)


#New z-scores for residuals for new model (with deleted points):
s<-summary(m2)$sigma
hii<-hatvalues(m2)
new.zres<-m2$residuals / (s*sqrt(1-hii))

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(CouponRate[-c(4,13,35)],new.zres,xlab="Coupon Rate", ylab="Standardized Residuals",xlim=c(2,14),ylim=c(-3.5,2.2), pch=19)
abline(h=-2,lty=2)
abline(h=2,lty=2)

#Illustrating calculation of Cook's D:
#Compare the model with one point deleted to the original model:
m3<-lm(bonds$BidPrice[-c(13)]~bonds$CouponRate[-c(13)])
#Plot both models on the same plot:
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(CouponRate, BidPrice,xlab="Coupon Rate", ylab="Bid Price",ylim=c(85,120),xlim=c(2,14), pch=19)
abline(m3)
abline(my.lm)
identify(CouponRate, BidPrice,Case)  


#On the other hand, if you delete point number 1, the two lines are almost exactly the same:
m3<-lm(bonds$BidPrice[-c(1)]~bonds$CouponRate[-c(1)])
#Plot both models on the same plot:
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(CouponRate, BidPrice,xlab="Coupon Rate", ylab="Bid Price",ylim=c(85,120),xlim=c(2,14), pch=19)
abline(m3)
abline(my.lm)
identify(CouponRate, BidPrice,Case)  


#The default plots that R produces for a model:
par(mar=c(4,4,3,3),  mfrow=c(2,2), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19)
plot(my.lm)

#Plot of Cook's D against x:
cd<-cooks.distance(my.lm)
par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19)
plot(CouponRate,cd, xlab="Coupon Rate", ylab="Cook's Distance")
abline(h=(4/33), lty=2)
