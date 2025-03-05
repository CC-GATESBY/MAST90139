library(faraway)
data(nes96)
help(nes96)
dim(nes96) #n=944, p=10
library(MASS)
head(nes96)
summary(nes96)
is.ordered(nes96$income)
levels(nes96$income)
levels(nes96$PID)
is.ordered(nes96$PID)

##############Define the response variable "party", and new data "rnes96"
party <- nes96$PID
levels(party) <- c("Democrat","Democrat","Independent","Independent","Independent","Republican","Republican")
inca <- c(1.5,4,6,8,9.5,10.5,11.5,12.5,13.5,14.5,16,18.5,21,23.5, 27.5,32.5,37.5,42.5,47.5,55,67.5,82.5,97.5,115)
income <- inca[unclass(nes96$income)]
table(nes96$income)
table(income)
rnes96 <- data.frame(party, income, education=nes96$educ, age=nes96$age)
summary(rnes96)
#################

pomod <- polr(party~age+education+income, data=rnes96, Hess=T,method="logistic")
pomod
summary(pomod)
anova(pomod)

fitted(pomod)
pomod$fitted
resid(pomod)  #does not exist.
pomod$residual
deviance(pomod)
pomod$deviance
pomod$lp  
predict(pomod,type="probs")
predict(pomod,type="class")
table(predict(pomod,type="class"))

rnes96$educ.f <-factor(unclass(rnes96$education))
pomodf <- polr(party~age+educ.f+income, data=rnes96, Hess=T,method="logistic")
summary(pomodf)
anova(pomod, pomodf)

pomod.aic <- step(pomod, k=2, trace=1)
pomod.bic <- step(pomod, k=log(944.0), trace=1)

anova(pomod, pomod.aic)

summary(pomod.aic)

predict(pomod.aic, rnes96[944, ],type="probs", se.fit=TRUE)

V<-solve(pomod.aic$Hess)
se.1<-sqrt(V[2,2]+115^2*V[1,1]-2*115*V[1,2]); 
se.2<-sqrt(V[3,3]+115^2*V[1,1]-2*115*V[1,3])

a<-pomod.aic$zeta-115*pomod.aic$coef
b1<-a-1.96*c(se.1,se.2)
b2<-a+1.96*c(se.1,se.2)

