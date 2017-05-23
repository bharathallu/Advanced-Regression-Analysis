rm(list=ls())
data<- read.csv(file="C:/STAT 608/HM02/boxoffice.csv",header = T,sep=",")
attach(data)
lm1<- lm(Current~Previous)
lm1
se<- summary(lm1)


ci<- lm1$coefficients[2]+(se$coefficients[2,2]*qt(.975,16))%*%c(-1,1)
ci_int<- lm1$coefficients[1]+(se$coefficients[1,2]*qt(.975,16))%*%c(-1,1)
names(se)
se$coefficients[2,2]

predict(lm1,data.frame(Previous=398))
predict(lm1,data.frame(Previous=398),interval = "confidence")
predict(lm1,data.frame(Previous=398),interval = "prediction")
?predict.lm
  
predict(lm1,data.frame(Previous))
x1<- rep(511.21,18)
x_1 <- as.matrix(x1,n.row=18)
plot(Previous)
