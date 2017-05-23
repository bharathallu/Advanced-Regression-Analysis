library(car)
library(alr3)

MichelinNY <- read.csv("/Users/home/Documents/TAMU/stat608/sp13/Notes/MichelinNY.csv", header=TRUE)
attach(MichelinNY)

y <- InMichelin
n<-dim(MichelinNY)[1]

#Box Plots:  Are the distributions different for 0 and 1?
#Need quadratic terms if variances differ!
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2), mfrow=c(2,2))
boxplot(Food~y, ylab="Food Rating",xlab="InMichGuide 1 = Yes")
boxplot(Decor~y, ylab="Decor Rating",xlab="InMichGuide 1 = Yes")
boxplot(Service~y, ylab="Service Rating",xlab="InMichGuide 1 = Yes")
boxplot(Price~y, ylab="Price",xlab="InMichGuide 1 = Yes")

#Run Box-Cox on the predictors:  Transform to multivariate normal
bc<-powerTransform(cbind(Food,Decor,Service,Price))
summary(bc)



#Our Model:
m1 <- glm(y~Food+Decor+Service+Price+log(Price),family=binomial(),data=MichelinNY)

#Marginal Model Plot
par(mfrow=c(2,2), pch=19, lwd=1.5)

#Food:
loessfit1 <- loess(y ~ Food, degree=1, span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Food, degree=1, span=2/3)
xx <- seq(15, 28.2, 0.05)
summary(m1)
plot(Food, y, xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Food=xx)))
lines(xx, predict(loessfit2,data.frame(Food=xx)), lty=2)

#Decor:
loessfit1 <- loess(y ~ Decor, degree=1, span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Decor, degree=1, span=2/3)
xx <- seq(10, 28.2, 0.05)
summary(m1)
plot(Decor, y, xlab="Decor Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Decor=xx)))
lines(xx, predict(loessfit2,data.frame(Decor=xx)), lty=2)

#Service:
loessfit1 <- loess(y ~ Service, degree=1, span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Service, degree=1, span=2/3)
xx <- seq(12, 28.2, 0.05)
summary(m1)
plot(Service, y, xlab="Service Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Service=xx)))
lines(xx, predict(loessfit2,data.frame(Service=xx)), lty=2)

#Price
loessfit1 <- loess(y ~ Price, degree=1, span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Price, degree=1, span=2/3)
xx <- seq(10, 200, 0.5)
summary(m1)
plot(Price, y, xlab="Price Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Price=xx)))
lines(xx, predict(loessfit2,data.frame(Price=xx)), lty=2)




#Do we need an interaction term?
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=1.5,cex.lab=1.5)
plot(Decor,Service,pch=y+1,col=y+1,xlab="Decor Rating",ylab="Service Rating")
abline(lsfit(Decor[y==0],Service[y==0]),lty=1,col=1)
abline(lsfit(Decor[y==1],Service[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Mich Guide?")


#Model with interaction
m2 <- glm(y~Food+Decor+Service+Price+log(Price) + Service:Decor, family=binomial(), data=MichelinNY)
par(mar=c(5.1,5.1,2,2), cex.axis=2,cex.lab=2)

#Can we drop the interaction term?  
summary(m2)
anova(m1,m2,test="Chisq")  #No; the interaction significantly reduces deviance.




#Figure 8.13 on page 291
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
hvalues <- influence(m2)$hat
stanresDeviance <- residuals(m2)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.7), pch=19)
abline(v=2*7/n,lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=1.2)



#Give up on keeping Price in the model; just keep log(Price).
m3 <- glm(y~Food+Decor+Service+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
anova(m3,m2,test="Chisq")
summary(m3)

#Check marginal model plots again:
par(mfrow=c(2,2), pch=19, lwd=1.5)
#Food:
loessfit1 <- loess(y ~ Food, degree=1, span=2/3)
loessfit2 <- loess(m3$fitted.values ~ Food, degree=1, span=2/3)
xx <- seq(15, 28.2, 0.05)
summary(m1)
plot(Food, y, xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Food=xx)))
lines(xx, predict(loessfit2,data.frame(Food=xx)), lty=2)

#Decor:
loessfit1 <- loess(y ~ Decor, degree=1, span=2/3)
loessfit2 <- loess(m3$fitted.values ~ Decor, degree=1, span=2/3)
xx <- seq(10, 28.2, 0.05)
summary(m1)
plot(Decor, y, xlab="Decor Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Decor=xx)))
lines(xx, predict(loessfit2,data.frame(Decor=xx)), lty=2)

#Service:
loessfit1 <- loess(y ~ Service, degree=1, span=2/3)
loessfit2 <- loess(m3$fitted.values ~ Service, degree=1, span=2/3)
xx <- seq(12, 28.2, 0.05)
summary(m1)
plot(Service, y, xlab="Service Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Service=xx)))
lines(xx, predict(loessfit2,data.frame(Service=xx)), lty=2)

#Price
loessfit1 <- loess(y ~ Price, degree=1, span=2/3)
loessfit2 <- loess(m3$fitted.values ~ Price, degree=1, span=2/3)
xx <- seq(10, 200, 0.5)
summary(m1)
plot(Price, y, xlab="Price Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx, predict(loessfit1,data.frame(Price=xx)))
lines(xx, predict(loessfit2,data.frame(Price=xx)), lty=2)



#And the leverage:
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
hvalues <- influence(m3)$hat
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.35), pch=19)
abline(v=2*6/n,lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=1.2)
#After removing Price, the old points of high leverage are no longer a problem.
#Atelier is "Closed" in the 2007 Zagat guide - Michelin inspectors rated it after departure of chef.

#Table of lucky and unlucky restaurants
fits4 <- m3$fitted.values
as.character(MichelinNY$Restaurant.Name)[c(14,37,69,133,135,138,160)]
round(fits4[c(14,37,69,133,135,138,160)],3)




detach(MichelinNY)
