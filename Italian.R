Italian<-read.csv("/Users/home/Documents/TAMU/stat608/Data/ItalianEdit.csv", header=TRUE)
attach(Italian)
plot(Italian)

m1<-lm(Food~Service + Pct_Liked + Cost)
anova(m1)
summary(m1)


m2<-lm(Food ~ Pct_Liked + Cost)
summary(m2)


m3<-lm(Food ~ Service)
anova(m3)

anova(m3, m1)  #anova(reduced, full)
#By hand, just to illustrate:
F.num <- (120.101 - 81.731)/(48 - 46)
F.denom <- 81.731 / 46
F <- F.num / F.denom
p.value <- 1 - pf(F, 2, 46)
F
p.value


#Using Matrix Notation:
library(car)
A.vec <-c(0, 0, 1, 0,
          0, 0, 0, 1)
A<-matrix(A.vec, nrow = 2, byrow=T)
linearHypothesis(model=m1, hypothesis.matrix=A, test="F")







#Chapter 6:  Marginal Model Plots
library(alr3)
m1<-lm(Food ~ Service + Cost + Decor)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,2,2), pch=19)
mmp(m1,Service)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3), pch=19)
mmp(m1, Decor)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3), pch=19)
mmp(m1,Cost)



