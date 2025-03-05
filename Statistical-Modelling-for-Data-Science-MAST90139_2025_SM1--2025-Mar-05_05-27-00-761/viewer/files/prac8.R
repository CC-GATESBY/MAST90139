##Q1
marr=read.csv("/Users/qguoqi/OneDrive - The University of Melbourne/My Documents/MAST90139/data/Mstatus.csv")
marr
#(a)
ms1=glm(freq~gend*PMS*EMS*MS, family=poisson, data=marr)
anova(ms1,test="Chi")

#(b)
scopems=list(lower=freq~gend*PMS*EMS, upper=freq~gend*PMS*EMS*MS)
step.ms2=step(ms1, scope=scopems)

anova(step.ms2,test="Chi")

#(c)
ms2=glm(freq~gend*PMS*EMS+PMS*EMS*MS+gend*MS, family=poisson, data=marr)
ms3=glm(freq~gend*PMS*EMS+PMS*EMS*MS, family=poisson, data=marr)
anova(ms3,ms2,test="Chi")

#(d)
marr2=marr[1:8, 6:10]
marr2

msb1=glm(freq.b/N.b~gend.b*PMS.b*EMS.b, family=binomial, weight=N.b, data=marr2)
anova(msb1, test="Chi")
summary(msb1)$coef

summary(ms1)$coef

msb2=glm(freq.b/N.b~gend.b+PMS.b*EMS.b, family=binomial, weight=N.b, data=marr2)
anova(msb2,msb1,test="Chi")

step(msb1)

anova(msb2,test="Chi")

summary(msb2)$coef

##############
##Q2

mobDen=read.csv("/Users/qguoqi/OneDrive - The University of Melbourne/My Documents/MAST90139/data/mobilityDenmark.csv")

mob1=glm(freq~factor(F)+factor(S)+diag+factor(dist),family=poisson,data=mobDen)
anova(mob1, test="Chi")

1-pchisq(12.41,12)

mob2=glm(freq~factor(F)+factor(S)+dist+factor(dist),family=poisson,data=mobDen)
anova(mob2, test="Chi")

mob3=glm(freq~factor(F)+factor(S)+dist+I(dist^2)+factor(dist),family=poisson,data=mobDen)
anova(mob3, test="Chi")

mob4=glm(freq~factor(F)+factor(S),family=poisson,data=mobDen)
matrix(resid(mob4), 5, 5)

mob5=glm(freq~factor(F)+factor(S)+diag,family=poisson,data=mobDen)
matrix(resid(mob5), 5, 5)

mob6=glm(freq~factor(F)+factor(S)+dist,family=poisson,data=mobDen)
matrix(resid(mob6), 5, 5)

mob7=glm(freq~factor(F)+factor(S)+dist+I(dist^2),family=poisson,data=mobDen)
matrix(resid(mob7), 5, 5)

mob8=glm(freq~factor(F)+factor(S)+factor(dist),family=poisson,data=mobDen)
matrix(resid(mob8), 5, 5)

summary(mob8)

