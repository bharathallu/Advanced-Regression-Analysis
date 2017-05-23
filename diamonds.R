library(car)
diamonds <-read.table("/Users/home/Documents/TAMU/stat608/Data/diamonds.txt", header=T)

x<-diamonds$Size
y<-diamonds$Price
m1 <- lm(y ~ x)

par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(diamonds$Size, diamonds$Price, xlab="Size", ylab="Price")
abline(m1, lwd=2)
identify(diamonds$Size, diamonds$Price)

predict(m1, interval="confidence", newdata=data.frame(x=c(0.25, 0.3)), level=0.95)
predict(m1, interval="prediction", newdata=data.frame(x=c(0.25, 0.3)), level=0.95)  #606.043552478826 735.366670048791

n <- dim(diamonds)[1] #Only okay for clean data - if you have missing observations, get df from model and add 2.
var.1 <- 998.4925342 #MSE from anova table.  Use print(anova(m1), digits=10) for more digits.
s <- sqrt(var.1) 
sxx <- var(x)*(n-1)
x.bar <- mean(x)
t.val <- qt(0.975, df=(n-2))
y.hat1.1 <- 670.7051112638
y.hat1.1 - t.val * s * sqrt(1 + (1/n) + (0.25-x.bar)^2/sxx)
y.hat1.1 + t.val * s * sqrt(1 + (1/n) + (0.25-x.bar)^2/sxx)

par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(2,2))
plot(m1)

cd1 <- cooks.distance(m1)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(diamonds$Size, cd1, xlab="Size")
abline(h=(4/49), lty=2)
identify(diamonds$Size, cd1)


par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(2,2))
plot(density(diamonds$Price,bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="Price")
plot(density(diamonds$Size,bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="Size")
plot(density(log(diamonds$Price),bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="Log(Price)")
plot(density(log(diamonds$Size),bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="Log(Size)")



#Try a log transformation
x<-log(diamonds$Size)
y<-log(diamonds$Price)
m2 <- lm(y ~ x)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(log(diamonds$Size), log(diamonds$Price))
abline(m2, lwd=2)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(2,2))
plot(m2)

cd2 <- cooks.distance(m2)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(log(diamonds$Size), cd2, xlab="log(Size)")
abline(h=(4/49), lty=2)

anova(m2)
predict(m2, interval="confidence", newdata=data.frame(x=c(log(0.25), log(0.3))), level=0.95)
exp(6.46291 + 0.0046/2)
exp(6.516589 + 0.0046/2)


#Use Box-Cox to choose a transformation.
bcx <- powerTransform(diamonds$Size)
summary(bcx)
invsize <- 1 / diamonds$Size
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(density(invsize,bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="1 / Size")

m3a <-lm(diamonds$Price ~ invsize)
bcy <- powerTransform(m3a)
summary(bcy)
invsqrtprice <- diamonds$Price^(-0.5)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(density(invsqrtprice,bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="Price ^ -0.5")

m3b <- lm(invsqrtprice ~ invsize)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(2,2))
plot(m3b)

cd3 <- cooks.distance(m3b)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(invsize, cd3, xlab="1 / Size")
abline(h=(4/49), lty=2)




#Weighted model:
x<-diamonds$Size
y<-diamonds$Price
m4 <- lm(y ~ x, weight = (1/x))
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(2,2))
plot(m4)

cd4 <- cooks.distance(m4)
par(cex.lab=1.4, cex.axis=1.4, pch=19, mfrow=c(1,1))
plot(diamonds$Size, cd4, xlab="Size")
abline(h=(4/49), lty=2)
identify(diamonds$Size, cd4)

#Yikes - the prediction interval formula is not quite right.  The confidence interval is okay, though.
predict(m4, interval="prediction", newdata=data.frame(x=c(0.25, 0.3)), level=0.95)
predict(m4, interval="confidence", newdata=data.frame(x=c(0.25, 0.3)), level=0.95)

#The way R is calculating the prediction intervals:
diff <- 683.159330477145 - 669.311816832035
t.val <- qt(0.975, df=(n-2))
sefit <- diff / t.val
var.4 <- 4972.67764845 #MSE from ANOVA table for model 4
y.hat1.4 <- 669.311816832035. #Center of prediction interval above
y.hat1.4 - t.val * sqrt(var.4 + a^2). #It assumed the variance of the individual at this value of x is the same as every other value.
y.hat1.4 + t.val * sqrt(var.4 + a^2)

#What we should be doing instead - divide var.4 by the weight for x*:
y.hat1.4 - t.val * sqrt(var.4*0.25 + a^2). #Remember the variance is weighted.
y.hat1.4 + t.val * sqrt(var.4*0.25 + a^2)
#Note that sometimes the interval will be wider, sometimes narrower than desired.




