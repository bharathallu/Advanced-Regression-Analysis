#30 4-week periods
#Koteswara Rao Kadiyala (1970) Testing for the independence of regression disturbances. Econometrica, 38, 97-117

icecream <- read.csv("/Users/home/Documents/TAMU/stat608/sp13/Notes/icecream.csv", header=TRUE)
attach(icecream)
n<-dim(icecream)[1]
library(nlme)
library(car)


par(mfrow=c(2,2), cex.axis=2, cex.lab=2, mar=c(4, 5, 2, 1))
plot(date, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(income, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(price, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(temp, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")

par(mfrow=c(1,2), cex.axis=2, cex.lab=2,  mar=c(4, 5, 4, 1), lwd=2, cex.main=2)
acf(IC, main="Consumption")
acf(temp, main="Temperature")

par(mfrow=c(2,2), cex.axis=2, cex.lab=2,  mar=c(4, 5, 2, 1))
plot(date, temp, col=(Year + 1), pch=(Year + 16))
plot(date, income, col=(Year + 1), pch=(Year + 16))
plot(date, price, col=(Year + 1), pch=(Year + 16))
plot(temp, price, col=(Year + 1), pch=(Year + 16))

par(mfrow=c(1,2), cex.axis=2, cex.lab=2,  mar=c(4, 5, 2, 1))
plot(temp, income, col=(Year + 1), pch=(Year + 16))
plot(income, price, col=(Year + 1), pch=(Year + 16))

#If we don't use autocorrelation of errors:
par(mfrow=c(1,1), cex.axis=2, cex.lab=2,  mar=c(4, 5, 4, 1), cex.main=1.5, lwd=2)
m1<-lm(IC ~ temp + price + income)
res1<-rstandard(m1)
acf(res1, main="Temperature not lagged")



#Run the transformation
m2g <- gls(IC ~ temp + price + income, correlation=corAR1(form = ~date), method="ML")


#Running by hand:
rho <- 0.7321772 
x <- model.matrix(m1)
iden <- diag(n)
Sigma <- rho^abs(row(iden)-col(iden))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% IC
m1tls <- lm(ystar ~ xstar - 1)
summary(m1tls)

#Diagnostic plots for the model:
StanRes2<- rstandard(m1tls)
par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot( date, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(income, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(price, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(temp, StanRes2, col=(Year + 1), pch=(Year + 16))

par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(m1tls)




#Try this model instead:
hist(income)
hist(income^(-3))

m3<- lm(IC ~  price + temp  + I(income^(-3)))
m3g <- gls(IC ~  price + temp + I(income^(-3)), correlation=corAR1(form = ~date), method="ML")
rho<-0.7288663  
x <- model.matrix(m3)
iden <- diag(n)
Sigma <- rho^abs(row(iden)-col(iden))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% IC
m3tls <- lm(ystar ~ xstar - 1)
summary(m3tls)

par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(m3tls)

StanRes3 <- rstandard(m3tls)
par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(date, StanRes3, col=(Year + 1), pch=(Year + 16))
plot(temp, StanRes3, col=(Year + 1), pch=(Year + 16))
plot(Year, StanRes3, col=(Year + 1), pch=(Year + 16))
plot(price, StanRes3, col=(Year + 1), pch=(Year + 16))


