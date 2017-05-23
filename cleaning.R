clean<-read.csv("/Users/home/Documents/TAMU/stat608/Data/clean.csv",header=TRUE)

attach(clean)

m1<-lm(Rooms ~ Crews)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3)) 
plot(Crews, Rooms, pch=19, xlab="Crews", ylab="Rooms")
abline(m1)

#Just to show that the simple linear model is terrible:
par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(m1)

#Let's create a better model:
x2<-sqrt(Crews)
y2<-sqrt(Rooms)
m2<-lm(y2~x2)

par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(m2)


#Now make confidence intervals for the regression line.
x2=seq(1, 4, by=0.1)
dt<-data.frame(x2)
ci<-predict(m2,newdata=dt,interval="confidence")

#Back-transform the end points properly.
anova(m2)   #To find Residual Mean Square (or MSE)
clb<-ci[,2]^2 + 0.353
cub<-ci[,3]^2 + 0.353

#If we're interested in #Crews = 4, x2 = sqrt(4) = 2, the 11th element:
#CI = (14.63, 18.22)




#Weighted Least Squares Regression:
cleanwt<-read.csv("/Users/home/Documents/TAMU/stat608/Data/cleanwt.csv",header=TRUE)
#I'm sorting the data set by the number of crews, from 2 to 16.
sorted <- cleanwt[order(cleanwt$Crews),] 


w.lm<- lm(sorted$Rooms ~ sorted$Crews, weights = (1/sorted$StdDev^2), x=TRUE)
pi<-predict(w.lm, weights = (1/sorted$StdDev^2), interval = "prediction")



par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(w.lm)


#Just for kicks, showing off the method of creating the "New" variables; gives same result as above:
#Multiply by sqrt(wi):
ynew <- sorted$Rooms/sorted$StdDev
x2new <- sorted$Crews/sorted$StdDev
x1new <- w.lm$x[,1]/sorted$StdDev  #The first column of the design matrix: a 1-vector.

w.lm2 <- lm(ynew ~ x1new + x2new - 1, x=TRUE)




#Just use matrix multiplication from scratch:
W <- diag(dim(w.lm$x)[1])*1/sorted$StdDev^2  #The diag function creates a diagonal matrix. 
X <- w.lm$x
beta.hat <- solve ( t(X)%*%W%*%X , t(X)%*%W%*%sorted$Rooms).  #try not to invert, then multiply.  This is more stable.


