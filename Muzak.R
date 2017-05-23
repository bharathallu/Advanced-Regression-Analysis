options(digits=15)
phone <- read.csv("/Users/home/Documents/TAMU/stat608/Data/Muzak.csv", header=T)
attach(phone)


m1 <- lm(HoldTime ~ Music, x=T)
summary(m1)
var.m1 <- anova(m1)$Mean[2]  #The Mean Square Error from the ANOVA table
X<- m1$x
solve(t(X)%*%X)

#Test for Muzak group different from 0: 
t.num1 <- (5.4 - 2.6)
t.denom1 <- sqrt(var.m1 * (0.2 + 0.4 - 2*0.2))
t.val1.1 <- t.num1 / t.denom1
p.val1.1 <- 2*pt(t.val1.1, df=12, lower.tail=FALSE) 
#If you get a p-value > 1, something went wrong.


#Test for difference between Ad group and Muzak group:  Use summary of lm.  Or by hand:
t.val1.2 <- (-2.6 - 0) / sqrt(var.m1 * 0.4)
p.val1.2 <- 2*pt(t.val1.2, 12)


#Test for difference between Classical group and Muzak group: 
t.num3 <- (5 + 2.6)
t.denom3 <- sqrt(var.m1 * (0.4 + 0.4 - 2 * 0.2))
t.val1.3 <- t.num3 / t.denom3
p.val1.3 <- 2*pt(t.val1.3, 12, lower.tail=F)






#Model 2: Create your own indicator variables.  
iAd <- ifelse(Music == "Ad", 1, 0)
iMuzak <- ifelse(Music == "Muzak", 1, 0)
iClassical <- ifelse(Music == "Classical", 1, 0)

m2 <- lm(HoldTime ~ iAd + iMuzak + iClassical - 1, x=T)
summary(m2)
var.m2 <- anova(m2)$Mean[4] #The anova table is different - more on this later.
X2<- m2$x
solve(t(X2)%*%X2)

#Test for Muzak group different from 0: Use summary of lm.  Or by hand:
t.val2.1 <- 2.8 / sqrt(var.m2 * 0.2)
p.val2.1 <- 2*pt(t.val2.1, 12, lower.tail=F)


#Test for difference between Ad group and Muzak group:
t.num4 <- (5.4 - 2.8)
t.denom4 <- sqrt(var.m2*(.2 + .2)).  #Notice no covariance between parameter estimates!
t.val2.2 <- t.num4 / t.denom4
p.val2.2 <- 2*pt(t.val2.2, 12, lower.tail=F)


#Test for difference between Classical group and Muzak group: 
t.num4 <- (10.4 - 2.8)
t.denom4 <- sqrt(var.m2*(.2 + .2))
t.val2.3 <- t.num4 / t.denom4
p.val2.3 <- 2*pt(t.val2.3, 12, lower.tail=F)
