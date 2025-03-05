library(faraway)
require(graphics)
data(swiss)
help(swiss)
dim(swiss); head(swiss)

####1.###
##numerical summary
summary(swiss)
cor(swiss)

#graphical summary
pairs(swiss, panel = panel.smooth, main = "swiss data",
      col = 3 + (swiss$Catholic > 50))

coplot(Fertility~Agriculture|Education, panel=panel.smooth, data=swiss)
##plot Fertility vs. Agriculture conditional on Education, where the
##values of Education are divided into six intervals indicated in the
##top panel. The order of the plots corresponding to the panel intervals
##is from the bottom and left to top and right.

coplot(Fertility~Agriculture|(Catholic > 50), panel=panel.smooth, data=swiss)

####2.###

######Fitting a linear model
lmod<-lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality, swiss)
summary(lmod)
drop1(lmod, test="F")
lmod1<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality, swiss)
summary(lmod1)
anova(lmod1, lmod)

lmodi<-lm(Fertility~(Agriculture+Education+Catholic+Infant.Mortality)^2, swiss)
summary(lmodi)

smallm <- step(lmodi,trace=FALSE, k=log(47)) #BIC. AIC if k=2
smallm
summary(smallm)
drop1(smallm, test="F")
anova(lmod1, smallm)

par(mfrow=c(2,2))
termplot(smallm,partial=T,terms=NULL)
plot(smallm)

#########3.
plot(density(swiss$Fertility),main="Fertility",xlab="Fertility")
rug(swiss$Fertility)
hist(swiss$Fertility,freq=F,add=T)

qqnorm(swiss$Fertility, ylab="Fertility")
qqline(swiss$Fertility)


plot(density((swiss$Fertility)^1.5),main="Fertility",xlab="Fertility")
rug((swiss$Fertility)^1.5)
hist((swiss$Fertility)^1.5,freq=F,add=T)

plot(density(log(swiss$Fertility)),main="Fertility",xlab="Fertility")
rug(log(swiss$Fertility))
hist(log(swiss$Fertility),freq=F,add=T)

plot(lmod1)
swiss$Fertility1.5 <- (swiss$Fertility)^1.5
lmodT<-lm(Fertility1.5~Agriculture+Education+Catholic+Infant.Mortality, swiss)
summary(lmodT)
plot(lmodT)

library(MASS)
Wlmodp<-lm(Fertility~Agriculture+Education+poly(Catholic,2)+
   Infant.Mortality + Education:poly(Catholic,2), swiss)
summary(Wlmodp)

Wlmodp1<-lm(Fertility~Agriculture+Education+poly(Catholic,2)+
   Infant.Mortality+Education:Catholic, swiss)
summary(Wlmodp1)

plot(Wlmodp1)
termplot(Wlmodp,partial=T,terms=NULL)

library(splines)
Wlmods<-lm(Fertility~Agriculture+Education+bs(Catholic,3)+
   Infant.Mortality + Education:bs(Catholic,3), swiss)
summary(Wlmods)

Wlmods1<-lm(Fertility~Agriculture+Education+bs(Catholic,3)+
   Infant.Mortality + Education:Catholic, swiss)
summary(Wlmods1)

plot(Wlmods1)
termplot(Wlmods,partial=T,terms=NULL)

halfnorm(hatvalues(lmod1))



#######4.
(1:47)[rownames(swiss)=="Sion"]  #38
(1:47)[rownames(swiss)=="Sierre"] #37
(1:47)[rownames(swiss)=="Porrentruy"] #6
(1:47)[rownames(swiss)=="Rive Gauche"] #47

swiss[c(6,37,38,47),]
rownames(swiss)[c(6,37,38,47)]
hatvalues(smallm)
influence.measures(smallm)
cooks.distance(smallm)
sort(cooks.distance(smallm))

#####5
pdf <- data.frame(Agriculture=mean(swiss$Agriculture), 
        Examination=mean(swiss$Examination),
        Education=mean(swiss$Education),
        Catholic=mean(swiss$Catholic),  
        Infant.Mortality=mean(swiss$Infant.Mortality))
pp <- predict(smallm,new=pdf)
pp

xtabs(round(pp,3) ~ econ + equip, pdf)

pdf <- data.frame(econ=rep("middle",15), equip=rep(levels(gavote$equip), rep(3,5)), perAA=rep(c(.11,0.23,0.35),5))
pp <- predict(finalm,new=pdf)

propAA <- gl(3,1,15,labels=c("low","medium","high"))
xtabs(round(pp,3) ~ propAA + equip,pdf)