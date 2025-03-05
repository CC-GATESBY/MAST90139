library(faraway)
help(hsb)
dim(hsb) #n=200, p=11
library(nnet)
head(hsb)
summary(hsb)
summary(hsb$prog)
is.factor(hsb$prog)
help(multinom)

hsb0 <-multinom(prog~1,data=hsb)
hsb0
summary(hsb0)
anova(hsb0)

hsb1<-multinom(prog~gender+race+ses+schtyp+read+write+math+science+socst,data=hsb, Hess=T)
hsb1
summary(hsb1)

Call:
multinom(formula = prog ~ gender + race + ses + schtyp + read + 
    write + math + science + socst, data = hsb)

Coefficients:
         (Intercept)  gendermale raceasian racehispanic racewhite     seslow
general     3.631901 -0.09264717  1.352739   -0.6322019 0.2965156 1.09864111
vocation    7.481381 -0.32104341 -0.700070   -0.1993556 0.3358881 0.04747323
         sesmiddle schtyppublic        read       write       math    science
general  0.7029621    0.5845405 -0.04418353 -0.03627381 -0.1092888 0.10193746
vocation 1.1815808    2.0553336 -0.03481202 -0.03166001 -0.1139877 0.05229938
               socst
general  -0.01976995
vocation -0.08040129

Std. Errors:
         (Intercept) gendermale raceasian racehispanic racewhite    seslow
general     1.823452  0.4548778  1.058754    0.8935504 0.7354829 0.6066763
vocation    2.104698  0.5021132  1.470176    0.8393676 0.7480573 0.7045772
         sesmiddle schtyppublic       read      write       math    science
general  0.5045938    0.5642925 0.03103707 0.03381324 0.03522441 0.03274038
vocation 0.5700833    0.8348229 0.03422409 0.03585729 0.03885131 0.03424763
              socst
general  0.02712589
vocation 0.02938212

Residual Deviance: 305.8705 
AIC: 357.8705 

attributes(hsb1)
attributes(summary(hsb1))

anova(hsb1) #need to specify two models for comparison in multinomial logit models.
#Note the goodness of fit or model adequacy test for a multinomial logit model
#cannot be reliably performed based on the residual deviance of the model.
#Rather it is based on smoothed deviance residuals of the model.
#R package "generalhoslem" has a function "logitgof" for doing this.

anova(hsb0,hsb1) #This compares two models based on the chi^2 test.
coef(hsb1)
coef(summary(hsb1))
hsb1$fitted
hsb1$residuals
hsb1$deviance
hsb1$edf
hsb1$AIC
hsb1$Hessian
predict(hsb1, hsb[102, ],type="probs")
#predict(hsb1, hsb[102, -c(1,6,7)],type="probs")
predict(hsb1, hsb[hsb$id==99, ],type="probs", se.fit=TRUE)

hsb1A <- multinom(prog~gender+race+ses+schtyp+read+write+math+science+socst,data=hsb, Hess=T)
hsb1A$Hess  #giving the observed information matrix, whose inverse 
           #is the variance matrix estimate of the beta estimates.
             
diag(solve(hsb1A$Hess))^(1/2) #giving standard errors of the beta.hat.

hsb1.aic <- step(hsb1, k=2, trace=1)
hsb1.aic
summary(hsb1.aic)

hsb1.bic <- step(hsb1, k=log(200.0), trace=1)
hsb1.bic

anova(hsb1, hsb1.aic)

###########
Likelihood ratio tests of Multinomial Models

Response: prog
                                                                 Model
1                                ses + schtyp + math + science + socst
2 gender + race + ses + schtyp + read + write + math + science + socst
  Resid. df Resid. Dev   Test    Df LR stat.   Pr(Chi)
1       386   315.5511                                
2       374   305.8705 1 vs 2    12 9.680566 0.6439616
#############################

V<-solve(hsb1.aic$Hessian)
dim(V)
sqrt(c(-1,-1)%*%V[c(2,4),c(2,4)]%*%c(-1,-1))  #for 6(a)

sqrt(c(-1,-1)%*%V[c(9,11),c(9,11)]%*%c(-1,-1)) #for 6(b)

sqrt(c(-1,-1,1,1)%*%V[c(2,4,9,11),c(2,4,9,11)]%*%c(-1,-1,1,1)) #for 6(c)

hsb[hsb$id==99,]
##    id gender  race  ses schtyp    prog read write math science socst
##102 99 female white high public general   47    59   56      66    61

####################################
predict(hsb1.aic, hsb[hsb$id==99, ],type="probs", se.fit=TRUE)

