bridge<-read.csv("/Users/home/Documents/TAMU/stat608/sp13/Notes/bridge.csv", header=TRUE)
library(MASS)
library(car)
library(leaps)
attach(bridge)


#Backward Selection Using P-values by hand:
full<-lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(full)

m1<-update(full, .~. - log(Length))
summary(m1)

m2<-update(m1, .~. - log(DArea))
summary(m2)

m3<-update(m2, .~. - log(CCost))
summary(m3)

#Final model: log(Time) ~ log(Dwgs) + log(Spans) 


##########################################################
#Forward selection by hand:
nothing<-lm(log(Time)~1)
addterm(nothing, scope=full, test="F")

m1<-lm(log(Time)~log(Dwgs))
addterm(m1, scope=full, test="F")

m2<-lm(log(Time)~log(Dwgs) + log(Spans))
addterm(m2, scope=full, test="F")

#Final model: log(Time) ~ log(Dwgs) + log(Spans) 


###########################################################
#Stepwise:  Use alpha to enter = 0.20, alpha to remove = 0.20
nothing<-lm(log(Time)~1)
addterm(nothing, scope=full, test="F")  #Forward Step

m1<-lm(log(Time)~log(Dwgs))
addterm(m1, scope=full, test="F")  #Forward Step

m2<-lm(log(Time)~log(Dwgs) + log(Spans))  #Backward Step?
summary(m2)

addterm(m2, scope=full, test="F")  #Forward Step






#Based on p-value, stop at model m2.  Based on AIC, we would have added log(CCost).
m3<-lm(log(Time)~log(Dwgs) + log(Spans) + log(CCost))  #Backward Step?
summary(m3)

extractAIC(m3,k=2)

addterm(m3, scope=full, test="F")  #Nothing else increases AIC.



##############################################################
#All Subsets:  (DON'T MISSPELL AS "SUBSET")
#Setting up full design matrix for later use:
X.labels<-c("LDArea", "LLength", "LDwgs", "LSpans", "LCCost")
X<-matrix(cbind(log(DArea), log(Length), log(Dwgs),log(Spans), log(CCost)), nrow=45, ncol=5, dimnames=list(c(1:45),X.labels))
b<-regsubsets(X, log(Time))
summary(b)


par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("adjr2"))

par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("bic"))


all.subs<-leaps(X, log(Time), method=c("adjr2"), nbest=1, names=X.labels)
all.subs$size. #Notice this gives the number of parameters, not the number of variables.
all.subs$which
all.subs$adjr2

om1<-lm(log(Time) ~ log(Dwgs))
om2<-lm(log(Time) ~ log(Dwgs) + log(Spans))
om3<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost))
om4<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost) + log(DArea))
om5<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost) + log(DArea) + log(Length))

#Subset size = 1:  
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))

#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))

#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))

#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))

#Subset size=5
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om5,k=log(n))







#Lasso:
mlasso<-lars(X, log(Time), type="lasso", normalize = FALSE, trace=TRUE)
summary.lars(mlasso)
coef(mlasso)
print(mlasso)
plot.lars(mlasso, xvar="step")



