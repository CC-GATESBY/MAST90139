library(faraway)
data(gavote, package="faraway")
help(gavote)
dim(gavote); head(gavote)

##numerical summary
summary(gavote)
###
gavote$undercount <- (gavote$ballots-gavote$votes)/gavote$ballots
###
summary(gavote$undercount)
with(gavote, sum(ballots-votes)/sum(ballots))

#graphical summary
par(mfrow=c(1,2))
hist(gavote$undercount,main="Undercount",xlab="Percent Undercount")
plot(density(gavote$undercount),main="Undercount")
rug(gavote$undercount)
pie(table(gavote$equip),col=gray(0:4/4))
barplot(sort(table(gavote$equip),decreasing=TRUE),las=2)
###
gavote$pergore <- gavote$gore/gavote$votes
###
plot(pergore ~ perAA, gavote, xlab="Proportion African American", ylab="Proportion for Gore")
plot(undercount ~ equip, gavote, xlab="", las=3)

xtabs(~ atlanta + rural, gavote)
names(gavote)
names(gavote)[4] <- "usage"

nix <- c(3,10,11,12)
cor(gavote[,nix])

######Fitting a linear model
lmod <- lm(undercount ~ pergore + perAA, gavote)
coef(lmod)
predict(lmod)
residuals(lmod)
deviance(lmod)
df.residual(lmod)
nrow(gavote)-length(coef(lmod))
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum <- summary(lmod); lmodsum$sigma
lmodsum$r.squared
cor(predict(lmod),gavote$undercount)^2
lmodsum$adj.r.squared
summary(lmod)
sumary(lmod)

########Interpretation
gavote$cpergore <- gavote$pergore - mean(gavote$pergore)
gavote$cperAA <- gavote$perAA - mean(gavote$perAA)
lmodi <- lm(undercount ~ cperAA+cpergore*usage+equip, gavote)
sumary(lmodi)

anova(lmod, lmodi)
drop1(lmodi, test="F")

confint(lmodi)
par(mfrow=c(2,2))
plot(lmodi)

gavote[cooks.distance(lmodi) > 0.1,]

par(mfrow=c(1,2))
halfnorm(hatvalues(lmodi))
gavote[hatvalues(lmodi)>0.3,]

termplot(lmodi,partial=TRUE,terms=1)

library(MASS)
rlmodi <- rlm(undercount ~ cperAA+cpergore*usage+equip, gavote)
summary(rlmodi)

wlmodi <- lm(undercount ~ cperAA+cpergore*usage+equip, gavote, weights=ballots)
sumary(wlmodi)

plmodi <- lm(undercount ~ poly(cperAA,4)+cpergore*usage+equip, gavote)
summary(plmodi)

par(mfrow=c(1,2))
termplot(plmodi,partial=TRUE,terms=1)

library(splines)
blmodi <- lm(undercount ~ cperAA+bs(cpergore,4)+usage+equip, gavote)
summary(blmodi)
termplot(blmodi,partial=TRUE,terms=2)

biglm <- lm(undercount ~ (equip+econ+usage+atlanta)^2+(equip+econ+usage+atlanta)*(perAA+pergore), gavote)
smallm <- step(biglm,trace=FALSE)

drop1(smallm,test="F")

finalm <- lm(undercount~equip + econ + perAA + equip:econ + equip:perAA, gavote)
sumary(finalm)

#######
pdf <- data.frame(econ=rep(levels(gavote$econ), 5), equip=rep(levels(gavote$equip), rep(3,5)), perAA=0.233)
pp <- predict(finalm,new=pdf)
xtabs(round(pp,3) ~ econ + equip, pdf)

pdf <- data.frame(econ=rep("middle",15), equip=rep(levels(gavote$equip), rep(3,5)), perAA=rep(c(.11,0.23,0.35),5))
pp <- predict(finalm,new=pdf)

propAA <- gl(3,1,15,labels=c("low","medium","high"))
xtabs(round(pp,3) ~ propAA + equip,pdf)