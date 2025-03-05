tox <-data.frame(dose=rep(2^(0:5),2), dosage=rep(0:5,2),
 sex=factor(rep(c("male","female"),each=6)), 
 killed=c(1,4,9,13,18,20,0,2,6,10,12,16), total=rep(20,12))

tox$dose.f=as.factor(tox$dose)

is.data.frame(tox)
is.factor(tox$Sex)

bud.1 <- glm(killed/total~sex+dose.f, family=binomial,weight=total, data=tox)
deviance(bud.1)
summary(bud.1)$coef

bud.2 <- glm(killed/total~sex*dose, family=binomial,weight=total, data=tox)
deviance(bud.2)
summary(bud.2)$coef

bud.3 <- glm(killed/total~sex+dose, family=binomial,weight=total, data=tox)
deviance(bud.3)
summary(bud.3)$coef

bud.4 <- glm(killed/total~sex+dose+I(dose^2), family=binomial,weight=total, data=tox)
deviance(bud.4)
summary(bud.4)$coef

bud.5 <- glm(killed/total~sex+dose+I(dose^2)+I(dose^3), family=binomial,weight=total, data=tox)
deviance(bud.5)
summary(bud.5)$coef

bud.6 <- glm(killed/total~sex*dosage, family=binomial,weight=total, data=tox)
deviance(bud.6)
summary(bud.6)$coef

bud.7 <- glm(killed/total~sex+dosage, family=binomial,weight=total, data=tox)
deviance(bud.7)
summary(bud.7)$coef

bud.8 <- glm(killed/total~sex+dosage+I(dosage^2), family=binomial,weight=total, data=tox)
deviance(bud.8)
summary(bud.8)$coef

bud.9 <- glm(killed/total~sex+dosage+I(dosage^2)+I(dosage^3), family=binomial,weight=total, data=tox)
deviance(bud.9)
summary(bud.9)$coef

