library(car)


prostateTraining <- read.table("/Users/home/Documents/TAMU/stat608/Data/prostateTraining.txt", header=TRUE)
prostateTest <- read.table("/Users/home/Documents/TAMU/stat608/Data/prostateTest.txt", header=TRUE)
prostateAlldata <- read.table("/Users/home/Documents/TAMU/stat608/Data/prostateAlldata.txt", header=TRUE)

attach(prostateTraining)

#Figure 7.2 on page 240
pairs(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)

#Figure 7.3 on page 241
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
StanRes1 <- rstandard(m1)
par(mfrow=c(3,3), pch=19)
plot(lcavol,StanRes1, ylab="Standardized Residuals")
plot(lweight,StanRes1, ylab="Standardized Residuals")
plot(age,StanRes1, ylab="Standardized Residuals")
plot(lbph,StanRes1, ylab="Standardized Residuals")
plot(svi,StanRes1, ylab="Standardized Residuals")
plot(lcp,StanRes1, ylab="Standardized Residuals")
plot(gleason,StanRes1, ylab="Standardized Residuals")
plot(pgg45,StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

#Figure 7.4 on page 241
par(mfrow=c(1,1), pch=19, cex=1.5)
plot(m1$fitted.values,lpsa,xlab="Fitted Values")
abline(lsfit(m1$fitted.values,lpsa))

#Figure 7.5 on page 242
par(mfrow=c(2,2), pch=19)
plot(m1)
abline(v=2*9/67,lty=2)


#Figure 7.6 page 243
library(alr3)
par(mfrow=c(3,3))
mmp(m1,lcavol,key=NULL)
mmp(m1,lweight,key=NULL)
mmp(m1,age,key=NULL)
mmp(m1,lbph,key=NULL)
mmp(m1,lcp,key=NULL)
mmp(m1,gleason,key=NULL)
mmp(m1,pgg45,key=NULL)
mmp(m1,m1$fitted.values,xlab="Fitted Values",key=NULL)



#R output on page 244
vif(m1)

#Figure 7.7 on page 244
#Check out the lweight plot.
par(mfrow=c(2,4))
avPlot(m1,variable=lcavol,ask=FALSE, main="")
avPlot(m1,variable=lweight,ask=FALSE, main="")
avPlot(m1,variable=age,ask=FALSE, main="")
avPlot(m1,variable=lbph,ask=FALSE, main="")
avPlot(m1,variable=svi,ask=FALSE, main="")
avPlot(m1,variable=lcp,ask=FALSE, main="")
avPlot(m1,variable=gleason,ask=FALSE, main="")
avPlot(m1,variable=pgg45,ask=FALSE, main="")


#Overall F-test is significant!
summary(m1)




#Figure 7.8 on page 245
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
subsets(b,statistic=c("adjr2"),min.size=1,max.size=4,cex.subsets=0.7)
subsets(b,statistic=c("adjr2"),min.size=5,max.size=8,cex.subsets=0.7,legend=FALSE)

#Table 7.2 on page 245
#Calculate adjusted R-squared
rs$adjr2
om1 <- lm(lpsa~lcavol)
om2 <- lm(lpsa~lcavol+lweight)
om3 <- lm(lpsa~lcavol+lweight+svi)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
om5 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45)
om6 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
om8 <- m1
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))

#....Etc.  
#BIC is smallest for Model2.
#AICC is smallest for Model 4.
#R^2_adj and AIC are smallest for Model 7.



#Regression output on page 246
summary(om2)
summary(om4)
summary(om7)

#Remember: p-values attained after model selection are much smaller than true values!!!!
#Which model would you pick if you had to stop here?

detach(prostateTraining)




attach(prostateTest)

#Regression output on page 247
om2 <- lm(lpsa~lcavol+lweight)
summary(om2)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
summary(om4)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
summary(om7)

detach(prostateTest)



attach(prostateTraining)

#Figure 7.9 on page 249
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
library(car)
subsets(b,statistic=c("adjr2"),main="With Case 45",min.size=1,max.size=5,cex.subsets=0.7)

m2 <- update(m1, subset=(1:67)[-c(45)])
lcavol1 <- lcavol[-c(45)]
lweight1 <- lweight[-c(45)]
age1 <- age[-c(45)]
lbph1 <- lbph[-c(45)]
svi1 <- svi[-c(45)]
lcp1 <- lcp[-c(45)]
gleason1 <- gleason[-c(45)]
pgg451 <- pgg45[-c(45)]
X <- cbind(lcavol1,lweight1,age1,lbph1,svi1,lcp1,gleason1,pgg451)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa[-c(45)])
rs <- summary(b)
library(car)
subsets(b,statistic=c("adjr2"),main="Without Case 45",min.size=1,max.size=5,cex.subsets=0.7,legend=FALSE)

detach(prostateTraining) 


attach(prostateAlldata)

#Figure 7.10 on page 249
par(mfrow=c(1,1))
plot(lweight[train==FALSE],lpsa[train==FALSE],pch=2,col=1,xlab="lweight",ylab="lpsa",ylim=c(-1,6),xlim=c(2,6.5))
abline(lsfit(lweight[train==FALSE],lpsa[train==FALSE]),lty=1,col=1)
points(lweight[train==TRUE],lpsa[train==TRUE],pch=3,col=2)
abline(lsfit(lweight[train==TRUE],lpsa[train==TRUE]),lty=2,col=2)
legend(4.5,2,legend=c("Training","Test"),pch=3:2,col=2:1,title="Data Set")

detach(prostateAlldata)


attach(prostateTest)

#Figure 7.11 on page 250
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
library(car)
par(mfrow=c(1,1))
avPlot(m1,variable=lweight,ask=FALSE,main="")

detach(prostateTest)
