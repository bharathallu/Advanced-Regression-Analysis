rm(list=ls())

## read data
data <- read.csv("C:/STAT 608/HM04/data_hm04.csv",header=T,sep=",")
attach(data)
lm1 <- lm(Suggested.retail.price ~ dealer.price,data = data)
summary(lm1)
par(mfrow=c(1,1))
plot(dealer.price,Suggested.retail.price,main = "Plot of dealer vs suggested price")
abline(lm1)

se<- summary(lm1)
ci_int<- lm1$coefficients[2]+(se$coefficients[2,2]*qt(.975,232))%*%c(-1,1)

par(mfrow=c(2,2))
plot(lm1)
vari <-anova(lm1)
sqrt(vari$`Mean Sq`[2])
lm2 <- lm(log(Suggested.retail.price) ~ log(dealer.price),data = data)
summary(lm2)
plot(lm2)
par(mfrow=c(1,1))
plot(log(dealer.price),log(Suggested.retail.price),main = "Plot of log dealer vs log suggested price")
abline(lm2)
par(mfrow=c(2,2))
plot(lm2)

## problem -7 ##
datas <- read.csv("C:/STAT 608/HM04/company.csv",header=T,sep=",")
attach(datas)
 plot(Sales,Assets,main="sales vs plots")
 lms <- lm(Assets~Sales)
 abline(lms)
 par(mfrow=c(2,2))
 plot(lms)
 
 par(mfrow=c(1,1))
 plot(Sales,type="b",main="plot of sales")
 plot(log(Sales),type="b",main="log of sales")
 hist(Sales,col="red")
 hist(log(Sales),col="red")
 
 
 par(mfrow=c(2,1))
 plot(Assets,type="b",main="plot of Assets")
 plot(log(Assets),type="b",main="log of Assets")
 hist(Assets,col="red")
 hist(log(Assets),col="red")
 
 
 
 lms1 <- lm(log(Assets)~log(Sales))
 summary(lms1)
 par(mfrow=c(2,2))
 plot(lms1)
 
 pred <- predict(lms1,data.frame(Sales=6571),interval = "confidence") 
 Assest_HP<- exp(3.4969+(0.5870*log(Sales[Company=="Hewlett-Packard"])))
 conf_HP <-  exp(c(pred[1],pred[2],pred[3]))
 
 h <- powerTransform(Sales)
 summary(h)
 i <- powerTransform(Assets)
 summary(i)
 
 
 ## problem-5 ##
 x= c(-3,-2,-1,0,6)
 y= c(10,6,5,3,12)
 par(mfrow=c(1,1))
 plot(x,y,xlim=c(-3,8),ylim=c(0,13))
 abline(a=7.2,b=0.5)
 identify(x,y,plot=T)
 y_hat <- 7.2 + (0.5*x)
 ei_hat <- abs(y - y_hat)
 hii <- (1/length(x)) + ((x-mean(x))^2/sum((x-mean(x))^2))
 hii[hii > (4/length(x))]
 
 x_mat <- matrix(c(1,1,1,1,1,-3,-2,-1,0,6),ncol=2)
 hat_mat <- x_mat%*%solve(t(x_mat)%*%x_mat)%*%t(x_mat)
diag(5)-hat_mat
std_res <- c(rep(0,5))
for(i in 1:length(x)) {
std_res[i] <- ei_hat[i]/(3.755*sqrt(1-hii[i]))
}
par(mfrow=c(1,1))
cd<-cooks.distance(lms)
par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19)
plot(Sales,cd, xlab="Sales", ylab="Cook's Dist")
abline(h=(4/79), lty=2)
identify(Sales,cd,plot=T)
dev.off()
