state<-read.csv("/Users/home/Documents/TAMU/stat608/sp13/Homework/statesp.csv", header=TRUE)

attach(state)

par(pch=19, cex=1.3, mar=c(4,4,1,1))
plot(MET,EX)
plot(GROW,EX)
plot(state)


library(car)
#Let's try Box-Cox.
#I do indeed need this family = "yjPower" option because there exist states with 0% in
#metropolitan areas.
#West is an indicator variable; doesn't need a power transformation.
X<-cbind(MET, ECAB, YOUNG, OLD)  #Stacks these four predictors into a single matrix.
tranx<-powerTransform(X, family="yjPower") #Searches for multivariate normality.
summary(tranx)


lm.1<-lm(EX ~ MET + ECAB + YOUNG + OLD + WEST)
tranmod <- powerTransform(lm.1, family="yjPower")
summary(tranmod)  #Looks like no transformation, fourth root, and log transformation are all acceptable.


library(alr3)
inverseResponsePlot(lm.1,key=TRUE)

y2<-EX^(1/4)
lm2<- lm(y2 ~ MET + ECAB + YOUNG + OLD + WEST)

par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lm2)

par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(MET, lm2$residuals)
plot(ECAB, lm2$residuals)
plot(YOUNG, lm2$residuals)
plot(OLD, lm2$residuals)

cd2 <- cooks.distance(lm2)
par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(1,1))
plot(lm2$fitted,cd2,xlab="Fitted Values", ylab="Cook's Distance")
abline(h=4/48,lty=2)
identify(lm2$fitted, cd2, STATE)




