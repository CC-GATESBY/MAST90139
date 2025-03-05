##Q1
freq <-c(175,116,131,17,160,126,135,21,132,120,154,29,13,19,40,5,5,9,33,3,22,29,110,6)
op <- factor(rep(1:4,6))
gender <- factor(rep(1:2,each=12))
year <-factor(rep(rep(1:3,each=4),2))

mod1 <- glm(freq~gender+year+op, family=poisson)
deviance(mod1)
summary(mod1)

mod2 <- glm(freq~gender*year+op, family=poisson)
summary(mod2)
mod3 <- glm(freq~gender*op+year, family=poisson)
summary(mod3)
mod4 <- glm(freq~gender+year*op, family=poisson)
summary(mod4)
mod5 <- glm(freq~gender*year+gender*op, family=poisson)
summary(mod5)
mod6 <- glm(freq~gender*year+year*op, family=poisson)
summary(mod6)
mod7 <- glm(freq~gender*op+year*op, family=poisson)
summary(mod7)
mod8 <- glm(freq~gender*year+gender*op+year*op, family=poisson)
summary(mod8)

##Treat yr as a numerical variable
yr<-rep(rep(1:3,each=4),2)
mod5a <- glm(freq~gender*year+gender*op+yr*op, family=poisson)
summary(mod5a)

mod9 <- glm(freq~year+op+year:op, family=poisson)
summary(mod9)
anova(mod9, test="Chi")

##collapse the table over gender
freqC <-freq[1:12]+freq[13:24]
opC <-factor(rep(1:4,3))
yearC <- factor(rep(1:3, each=4))

mod10 <-glm(freqC ~ yearC +opC, family=poisson)
summary(mod10)

###Q2
Y <- c(2, 3, 3, 4, 3, 2, 0, 3, 8, 11, 6, 6, 7, 12, 11, 11)
N <- c(119, 124, 50, 26, 88, 100, 43, 23, 127, 220, 74, 49, 74, 111, 57, 44)
BP <- factor(rep(1:4, 4))
CHOL <- factor(rep(1:4, rep(4, 4)))

fit.1 <- glm(Y/N ~ 1, weights = N, family = "binomial")
summary(fit.1)
fit.2 <- glm(Y/N ~ CHOL, weights = N, family = "binomial")
summary(fit.2)
fit.3 <- glm(Y/N ~ BP, weights = N, family = "binomial")
summary(fit.3)
fit.4 <- glm(Y/N ~ CHOL + BP, weights = N, family = "binomial")
summary(fit.4)


Freq<- c(Y, N-Y)
CHD <- factor(rep(1:2, each=16))
BP1<- factor(rep(1:4,8))
CHOL1 <- factor(rep(rep(1:4,each=4),2))

fitP.1 <- glm(Freq ~ CHOL1*BP1+CHD, family=poisson)
summary(fitP.1)
deviance(fitP.1)
anova(fitP.1, test="Chi")
fitP.2 <- glm(Freq ~ BP1+CHD*CHOL1, family=poisson)
summary(fitP.2)
anova(fitP.2)
fitP.3 <- glm(Freq ~ CHOL1+ CHD*BP1, family=poisson)
summary(fitP.3)
anova(fitP.3)
fitP.4 <- glm(Freq ~ CHOL1*BP1+CHD*CHOL1+CHD*BP1, family=poisson)
summary(fitP.4)
anova(fitP.4)
anova(fit.4)

fitP.5 <- glm(Freq ~ BP1+CHOL1+CHD, family=poisson)
summary(fitP.5)
anova(fitP.5)
fitP.6 <- glm(Freq ~ CHOL1*BP1+CHOL1*CHD, family=poisson)
deviance(fitP.6)
fitP.7 <- glm(Freq ~ CHOL1*BP1+BP1*CHD, family=poisson)
deviance(fitP.7)





