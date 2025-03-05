##This script is used to analyse Examples the breathing test results.
library(MASS)
BTR.dat <- read.csv("D:/MAST90139/Example6-2BTR.csv")
is.data.frame(BTR.dat)
BTR.dat
is.factor(BTR.dat$BTR)
is.ordered(BTR.dat$BTR)
is.factor(BTR.dat$Age)
is.factor(BTR.dat$Smoking)
is.factor(BTR.dat$Freq)
BTR.dat$BTR=as.ordered(BTR.dat$BTR)

######################################Usage of polr()
#####Ordered Logistic or Probit Regression
polr(formula, data, weights, start, ..., subset, na.action,
     contrasts = NULL, Hess = FALSE, model = TRUE,
     method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))

###Note the signs of beta.hat are opposite to those given in the textbook.
#########################################################

#options(contrasts = c("contr.treatment", "contr.poly"))
options(contrasts = c("contr.sum", "contr.poly"))

######Fit a cumulative logistic or proportional odds model:################
BTR.logit=polr(BTR~Age+Smoking+Age:Smoking, data=BTR.dat, weights=Freq, Hess=T,method="logistic")
summary(BTR.logit)

BTR.logit1=polr(BTR~Age+Smoking, data=BTR.dat, weights=Freq, Hess=T,method="logistic")
summary(BTR.logit1)

anova(BTR.logit, BTR.logit1)
######Fit a grouped Cox or proportional hazards model:################
BTR.ph=polr(BTR~Age+Smoking+Age:Smoking,data=BTR.dat, weights=Freq, Hess=T, method="cloglog")
summary(BTR.ph)

######Fit a cumulative model with the extreme maximal-value distribution:################
BTR.extm=polr(BTR~Age+Smoking+Age:Smoking,data=BTR.dat,weights=Freq, Hess=T,method="loglog")
summary(BTR.extm)

step(BTR.logit)
anova(BTR.logit, BTR.ph) 

BTR.logit$fitted
BTR.logit$residual  #does not exist.
BTR.logit$deviance
BTR.logit$lp  #linear predictor values

vcov(BTR.logit)  #variance matrix of parameter estimates, approximation of the Hessian inverse.
solve(BTR.logit$Hess) #
predict(BTR.logit,BTR.dat,type="probs")
predict(BTR.logit,type="probs") #same results as from the above.
predict(BTR.logit,type="class", se.fit=TRUE) #se.fit option does not work.

model.frame(BTR.logit)
profile(BTR.logit)

####################################
####################################
The following is about: 
Generate correlated Bernoulli random numbers.
#######################################
x=rbinom(100,1,0.6)
y=rbinom(100,1,0.7)
cor(x,y)
rBiBernoulli=function(n, p1,p2, gamma){
#n; sample size
#p1=Pr(X1=1), p2=Pr(X2=1), gamma=odds ratio, should not equal 1.
#This function generates a sample of correlated Bernoulli random numbers).
s1=sqrt((1-(p1+p2)*(1-gamma))^2-4*(gamma-1)*gamma*p1*p2)
p11=(1-(p1+p2)*(1-gamma)-s1)/(2*(gamma-1))
p10=p1-p11
p01=p2-p11
p00=1-p11-p10-p01
cor1=(p11-p1*p2)/sqrt(p1*(1-p1)*p2*(1-p2))
cat1=sample(1:4,n,replace=TRUE,prob=c(p00,p01,p10,p11))
X=matrix(0,n,2)
for(i in 1:n)X[i,]=switch(cat1[i], c(0,0), c(0,1), c(1,0), c(1,1))
list(data=X,p00=p00,p01=p01,p10=p10,p11=p11,cor=cor1)
}
X=rBiBernoulli(400000,0.3, 0.8,2)
X$cor
cor(X$data)

