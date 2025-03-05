library(faraway)

###2.1 Heart disease example
data(wcgs, package="faraway")
help(wcgs)
dim(wcgs); head(wcgs)

summary(wcgs[,c("chd","height","cigs")])
summary(wcgs)

par(mfrow=c(1,2))
plot(height ~ chd, wcgs)
wcgs$y <- ifelse(wcgs$chd == "no",0,1)
plot(jitter(y,0.1) ~ jitter(height), wcgs, xlab="Height", ylab="Heart Disease", pch=".")

library(ggplot2)
ggplot(wcgs, aes(x=height, color=chd)) + geom_histogram(position="dodge", binwidth=1)
ggplot(wcgs, aes(x=cigs, color=chd)) + geom_histogram(position="dodge", binwidth=5, aes(y=..density..))

ggplot(wcgs, aes(x=height,y=cigs))+geom_point(alpha=0.2, position=position_jitter())+facet_grid(~ chd)

curve(ilogit(x),-6,6, xlab=expression(eta), ylab="p")

lmod <- glm(chd ~ height + cigs, family = binomial, wcgs)
summary(lmod)
beta <- coef(lmod)
sumary(lmod)

par(mfrow=c(1,2))
plot(jitter(y,0.1) ~ jitter(height), wcgs, xlab="Height", ylab="Heart Disease",pch=".")
curve(ilogit(beta[1] + beta[2]*x + beta[3]*0),add=TRUE)
curve(ilogit(beta[1] + beta[2]*x + beta[3]*20),add=TRUE,lty=2)

plot(jitter(y,0.1) ~ jitter(cigs), wcgs, xlab="Cigarette Use", ylab="Heart Disease",pch=".")
curve(ilogit(beta[1] + beta[2]*60 + beta[3]*x),add=TRUE)
curve(ilogit(beta[1] + beta[2]*78 + beta[3]*x),add=TRUE,lty=2)

exp(beta)

lmodc <- glm(chd ~ cigs, family = binomial, wcgs)
anova(lmodc,lmod, test="Chi")

drop1(lmod,test="Chi")

confint(lmod)

linpred <- predict(lmod)
predprob <- predict(lmod, type="response")
head(linpred)
head(predprob)
head(ilogit(linpred))
rawres <- wcgs$y - predprob
head(rawres)
head(residuals(lmod, type="response"))
head(residuals(lmod, type="deviance"))
head(residuals(lmod))
head(residuals(lmod, type="pearson"))

###Create Figure 2.6
par(mfrow=c(1,2))
plot(rawres ~ linpred, xlab="linear predictor", ylab="raw residuals")

library(dplyr) #Creat the binned residual plot

wcgs <- mutate(wcgs, residuals=residuals(lmod), linpred=predict(lmod))
#The next step is to create the bins:
gdf <- group_by(wcgs, cut(linpred, breaks=unique(quantile(linpred,(1:100)/101))))
#The above line does not work because of missing values NA in some predictors.
gdf <- group_by(wcgs, ntile(linpred,100))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
#The residuals plot can be seen in the second panel of Figure 2.6 as:
plot(residuals ~ linpred, diagdf, xlab="linear predictor")

###Create Figure 2.7
gdf <- group_by(wcgs, height)
diagdf <- summarise(gdf, residuals=mean(residuals))
ggplot(diagdf, aes(x=height,y=residuals)) + geom_point()

filter(wcgs, height==77) %>% select(height, cigs, chd, residuals)
group_by(wcgs, cigs) %>% summarise(residuals=mean(residuals), count=n()) %>% ggplot(aes(x=cigs, y=residuals, size=sqrt(count)))+geom_point()

####Figure 2.8
qqnorm(residuals(lmod))
halfnorm(hatvalues(lmod))
filter(wcgs, hatvalues(lmod) > 0.015) %>% select(height, cigs, chd)

drop1(glm(chd ~ dbp, family=binomial, wcgs), test="Chi")

wcgs$bmi <- with(wcgs, 703*wcgs$weight/(wcgs$height^2))

dim(wcgs)
## [1] 3154   15
sum(is.na(wcgs))
#[1] 14   #12 missing chol, 2 missing arcus

wcgsm <- na.omit(wcgs) #dim(wcgsm)=c(3140,15)

lmod <- glm(chd ~ age + height + weight +bmi + sdp + dbp + chol + dibep + cigs +arcus, family=binomial, wcgsm)
lmodr <- step(lmod, trace=0)
sumary(lmodr)

drop1(lmod, test="Chi")
drop1(glm(chd ~ age + height + weight + bmi + sdp + chol + dibep + 
    cigs + arcus, family=binomial, wcgsm), test="Chi")

drop1(glm(chd ~ age + height + bmi + sdp + chol + dibep + 
    cigs + arcus, family=binomial, wcgsm), test="Chi")

drop1(glm(chd ~ age + height + bmi + sdp + chol + dibep + cigs, family=binomial, wcgsm), test="Chi")

drop1(glm(chd ~ age + bmi + sdp + chol + dibep + cigs, family=binomial, wcgsm), test="Chi")


lmodBE <- glm(formula = chd ~ age + bmi + sdp + chol + dibep + 
    cigs, family = binomial, data = wcgsm)
lmodAIC <- glm(formula = chd ~ age + height + bmi + sdp + chol + dibep + 
    cigs + arcus, family = binomial, data = wcgsm)
anova(lmodBE,lmodAIC, test="Chi")

wcgsm <- na.omit(wcgs)
wcgsm <- mutate(wcgsm, predprob=predict(lmodAIC,type="response"), linpred=predict(lmodAIC))
#linpred <-predict(lmodAIC)
gdf <- group_by(wcgsm, cut(linpred, breaks=unique(quantile(linpred,(1:100)/101))))
#The above command does not work because of implicit NAs
gdf <- group_by(wcgsm, ntile(linpred, 100))

hldf <- summarise(gdf, y=sum(y), ppred=mean(predprob), count=n())

hldf <- mutate(hldf, se.fit=sqrt(ppred*(1-ppred)/count))
ggplot(hldf,aes(x=ppred,y=y/count,ymin=y/count-2*se.fit,ymax=y/count+2*se.fit))+geom_point()+geom_linerange(color=grey(0.75))+geom_abline(intercept=0,slope=1)+xlab("Predicted Probability")+ylab("Observed Proportion")

hlstat <- with(hldf, sum( (y-count*ppred)^2/(count*ppred*(1-ppred))))
c(hlstat, nrow(hldf))
# [1]  94.28042 100.00000
1-pchisq(94.28042, 100-1)
#[1] 0.6153527

wcgsm <- mutate(wcgsm, predout=ifelse(predprob < 0.5, "no", "yes"))
xtabs( ~ chd + predout, wcgsm)

thresh <- seq(0.01,0.5,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))
for(j in seq(along=thresh)){
pp <- ifelse(wcgsm$predprob < thresh[j],"no","yes")
xx <- xtabs( ~ chd + pp, wcgsm)
Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
Sensitivity[j] <- xx[2,2]/(xx[2,1]+xx[2,2])
}

par(mfrow=c(1,2))
matplot(thresh,cbind(Sensitivity,Specificity),type="l",xlab="Threshold",ylab="Proportion",lty=1:2)

plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)
AUCv <- numeric(length(thresh)-1)
for(i in 1:(length(thresh)-1)){AUCv[i]<-0.5*sum(Sensitivity[i:(i+1)])*(Specificity[i+1]-Specificity[i])}
AUC <- sum(AUCv)

