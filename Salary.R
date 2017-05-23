salary<-read.csv("/Users/home/Documents/TAMU/stat608/sp13/Notes/profsalary.csv",header=TRUE)


y1<-salary$Salary
x1<-salary$Experience

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, y1, xlab="Years Experience", ylab="Salary")

m1<-lm(y1~x1)

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, rstandard(m1), xlab="Years Experience", ylab="Standardized Residuals")


#We may need to use the I() function when fitting polynomial terms:
m2<-lm(y1~x1+I(x1^2))
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, rstandard(m2), xlab="Years Experience", ylab="Standardized Residuals")

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, hatvalues(m2), xlab="Years Experience", ylab="Leverage")
abline(h=(6/length(x1)), lty=2)


par(cex.main=1.5, cex.axis = 1.5, cex.lab=1.5, mar=c(10,5,2,3), pch=19, mfrow=c(2,2))
plot(m2)





#Chapter 6: Marginal Model Plots
library(car)

attach(salary)
m1<-lm(Salary ~ Experience)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3))
mmp(m1,Experience, xlab="Years of Experience")

m2<-lm(Salary ~ Experience + I(Experience^2))
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3))
mmp(m2,Experience,xlab="Years of Experience")




