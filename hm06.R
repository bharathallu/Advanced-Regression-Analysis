rm(list=ls())
data <- read.csv("C:/STAT 608/HM06/statesp.csv",sep=",",header=T)
attach(data)
m1 <- lm(EX ~ MET)
summary(m1)
plot(MET^2,EX)
m2 <- lm(EX ~ ECAB+MET+YOUNG+OLD+WEST+GROW+I(MET^2))
summary(m2)
m3 <- lm(EX ~ ECAB+MET+WEST+GROW+I(MET^2))
summary(m3)
anova(m3,m2)

rm(list=ls())
data <- read.csv("C:/STAT 608/HM06/overdue.csv",sep=",",header=T)
attach(data)
m1 <- lm(LATE ~ BILL,data = data[1:48,])
summary(m1)
m2 <- lm(LATE ~ BILL,data = data[49:96,])
summary(m2)

colvec <- rep("red",96)
colvec[Type=="residential"]="blue"
plot(BILL,LATE,col=colvec,main="Late vs Bill")
abline(lm(LATE ~ BILL,data = data[1:48,]))
abline(lm(LATE ~ BILL,data = data[49:96,]))

data$new <- rep(0,96)
data$new[Type=="commercial"]= 1
data$new <- as.factor(data$new)


m5 <- lm( LATE ~ BILL*data$new)
summary(m5)
