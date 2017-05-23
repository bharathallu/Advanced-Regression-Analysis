rm(list=ls())
pga <- read.csv('C:/STAT 608/HM07/pgatour2006.csv',sep=',',header = T)
attach(pga)
lm1 <- lm(PrizeMoney ~ DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga)
summary(lm1)
library(car)
vif(lm1)
par(mfrow=c(2,2))
plot(lm1)
x <- cbind(DrivingAccuracy,GIR,PuttingAverage,BirdieConversion,SandSaves,Scrambling,PuttsPerRound)
box <- powerTransform(x,family='yjPower')
summary(box)
summary(powerTransform(lm1))
lm2 <- lm(log(PrizeMoney) ~ DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga)
summary(lm2)
vif(lm2)
plot(lm2)
lm3<- lm(log(PrizeMoney) ~ DrivingAccuracy+GIR+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga)
summary(lm3)
vif(lm3)
plot(lm3)

library(leaps)
mod <- regsubsets(log(PrizeMoney)~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga,nvmax=8)
mod.summary <- summary(mod)
mod.summary$adjr2

coef(mod,5)
mod1 <- regsubsets(log(PrizeMoney)~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga,nvmax=8,method = "forward")
mod1.summary <- summary(mod1)
mod1.summary$cp
mod1.summary$bic

library(glmnet)
x <- model.matrix(log(PrizeMoney)~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound,pga)[,-1]
mod.lasso <- cv.glmnet(x,pga$PrizeMoney,alpha=1)
lasso.mod.coeff <- predict(mod.lasso,s=3000,type="coefficients")[1:8,]

detach(pga)

select <- read.csv('C:/STAT 608/HM07/mselect.csv',header=T,sep=',')
pairs(select)
attach(select)
lm4 <- lm(y~ (x1+x2+x3+x4+x5)**2+I(x1^2)+I(x2^2)+I(x3^2)+I(x4^2)+I(x5^2),select)
lm5 <- lm(y ~ (x1+x2+x3+x4+x5)**2,select)
summary(lm5)
lm6 <- lm(y~ x1+x2+x3+x4+x5+I(x2^2)+x2:x5+x3:x4,select)
anova(lm6,lm4)

mod1 <- regsubsets(y~ (x1+x2+x3+x4+x5)**2+I(x1^2)+I(x2^2)+I(x3^2)+I(x4^2)+I(x5^2),select,nvmax=30,method = "backward")
which.min(summary(mod1)$bic)
coef(mod1,6)

lm6 <- lm(y~ x1+x2+x3+x4+x3:x4,select)
summary(lm6)
vif(lm6)
