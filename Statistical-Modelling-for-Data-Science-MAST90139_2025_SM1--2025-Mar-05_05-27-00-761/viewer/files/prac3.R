library(faraway)

###2.1 Pima data example
data(pima, package="faraway")
help(pima)
dim(pima); head(pima)

summary(pima)

######(a)######
pima$test.f <- factor(pima$test)
levels(pima$test.f) <- c("negative","positive")
pima[1,]

par(mfrow=c(1,2))
plot(insulin ~ test.f, pima)
plot(jitter(test,0.1) ~ jitter(insulin), pima, xlab="insulin", ylab="signs of diabetes", pch="*")

#################
library(ggplot2)
ggplot(pima, aes(x=insulin, color=test.f)) + 
 geom_histogram(position="dodge", binwidth=30)

ggplot(pima, aes(x=insulin, color=test.f)) + 
  geom_histogram(position="dodge", binwidth=10, aes(y=..density..))

summary(pima$insulin)
summary(pima$test.f[pima$insulin==0])

#########(b)########
pima$insulinN <- pima$insulin
pima$insulinN[pima$insulin==0]<-NA
summary(pima$insulinN)
summary(pima$insulinN[pima$test.f=="negative"])
summary(pima$insulinN[pima$test.f=="positive"])

#################
ggplot(pima, aes(x=insulinN, color=test.f)) + 
 geom_histogram(position="dodge", binwidth=30)

ggplot(pima, aes(x=insulinN, color=test.f)) + 
  geom_histogram(position="dodge", binwidth=30, aes(y=..density..))

#################
################(c)###################
pima$pregnantN=pima$pregnant
pima$pregnantN[pima$pregnant==0]<-NA
summary(pima$pregnantN)
table(pima$pregnant)

pima$glucoseN=pima$glucose
pima$glucoseN[pima$glucose==0.0]<-NA
summary(pima$glucoseN)

pima$diastolicN=pima$diastolic
pima$diastolicN[pima$diastolic==0.0]<-NA
summary(pima$diastolicN)

pima$tricepsN=pima$triceps
pima$tricepsN[pima$triceps==0.0]<-NA
summary(pima$tricepsN)

pima$bmiN=pima$bmi
pima$bmiN[pima$bmi==0.0]<-NA
summary(pima$bmiN)
summary(pima)
#####################

lmod <- glm(test ~ pregnant+glucose+diastolic+triceps+insulin+bmi+
       diabetes+age, family = binomial, pima)
summary(lmod)

lmodNA <- glm(test ~ pregnantN+glucoseN+diastolicN+tricepsN+insulinN+bmiN+
       diabetes+age, family = binomial, pima)
summary(lmodNA)

lmodN <- glm(test ~ pregnant+glucoseN+diastolicN+triceps+insulinN+bmiN+
       diabetes+age, family = binomial, pima)
summary(lmodN)
##############################

#############(d)#########
lmodNAA <- glm(test ~ pregnantN+glucoseN+diastolicN+bmiN+
       diabetes+age, family = binomial, pima)
summary(lmodNAA)

pimaN <- na.omit(pima)
dim(pimaN)

lmodNA1 <- glm(test ~ pregnantN+glucoseN+diastolicN+tricepsN+insulinN+bmiN+
       diabetes+age, family = binomial, pimaN)
summary(lmodNA1)

lmodNA2 <- glm(test ~ pregnantN+glucoseN+diastolicN+bmiN+
       diabetes+age, family = binomial, pimaN)
summary(lmodNA2)

anova(lmodNA1, lmodNA2, test="Chi")

anova(lmodNA2, lmodNA1, test="Chi")
############

################(e)############
lmodNAr <- step(lmodNA1, trace=0)
summary(lmodNAr)
##############

##########(f)#########
pima$misInd<-apply(pima,1, anyNA)
xtabs(~test.f+misInd, pima)

summary(glm(test.f~misInd, family=binomial, pima))
anova(glm(test.f~misInd, family=binomial, pima), test="Chi")
chisq.test(pima$test.f, pima$misInd, correct=F)

lmodNArs <- glm(test ~ glucoseN + bmiN + diabetes + age, family = binomial, 
    data = pima)
summary(lmodNArs)
#############

################(g)############
##Q1(bmiN)=27.5; Q3(bmiN)=36.6
## log-odds difference = beta2*(Q3-Q1)= 0.086372*(36.6-27.50)=0.7859852
##odds ratio (OR)=exp(0.086372*(36.6-27.50))=2.194568
## 95% CI for the log-odds difference =
### c(0.7859852-1.96*0.014448*(36.6-27.5), 0.7859852+1.96*0.014448*(36.6-27.5))
### = (0.5282907, 1.0436797)
### 95% C.I. for the OR = (exp(0.5282907), exp(1.0436797))=(1.696031, 2.839647)
########################

###########(h)############
summary(pima$diastolicN[pima$test.f=="negative"])
summary(pima$diastolicN[pima$test.f=="positive"])
##Diastolic values tend to be higher for those positives.

library(ggplot2)
ggplot(pima, aes(x=diastolicN, color=test.f)) + 
 geom_histogram(position="dodge", binwidth=10)

ggplot(pima, aes(x=diastolicN, color=test.f)) + 
  geom_histogram(position="dodge", binwidth=10, aes(y=..density..))
##The interleaved histograms of the diastolicN between those testing 
##positive and negative do not seem to be significantly different. 

t.test(diastolic~test.f, alternative="less",data=pima, var.equal=T)
wilcox.test(diastolic~test.f, alternative="less",data=pima)
##Both tests return very small p-values.

###On the other hand, from the previous logistic regression models, we see
###the effects of diastolicN on the test result is not significant.

