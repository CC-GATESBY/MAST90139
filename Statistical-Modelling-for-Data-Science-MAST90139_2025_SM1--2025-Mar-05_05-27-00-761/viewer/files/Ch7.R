#########################
library(lme4)
data(sleepstudy)
head(sleepstudy)
str(sleepstudy)
sleepstudy$Subject

require(lattice)
xyplot(Reaction ~ Days | Subject, data=sleepstudy,
type = c("g","p","r"),
xlab = "Days of sleep deprivation",
ylab = "Average reaction time (ms)", aspect = "xy")

#Random intercept model
fit1 <- lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
summary(fit1)
VarCorr(fit1)

names(fit1)
getSlots("mer")

fit1@deviance

sum.fit1 <- summary(fit1) # Additional information returned by summary
class(sum.fit1)
getSlots("summary.mer")
sum.fit1@coefs # Coefficients
VarCorr(fit1) #

# Model Diagnostics
y.hat <- fitted(fit1) # Fitted values
int.hat <- ranef(fit1)[[1]][[1]] # Predicted intercepts
res.hat <- residuals(fit1) # Estimated residuals

par(mfrow=c(1,3))
qqnorm(int.hat, main="Random Intercepts"); qqline(int.hat)

qqnorm(res.hat, main="Residuals"); qqline(res.hat)
plot(y.hat, res.hat, xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty=2)

fit2 <- lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
summary(fit2)

VarCorr(fit2) #

anova(fit1, fit2)

refitting model(s) with ML (instead of REML)
Data: sleepstudy
Models:
fit1: Reaction ~ Days + (1 | Subject)
fit2: Reaction ~ Days + (Days | Subject)
     Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
fit1  4 1802.1 1814.8 -897.04   1794.1                             
fit2  6 1763.9 1783.1 -875.97   1751.9 42.139      2  7.072e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

y.hat2 <- fitted(fit2) # Fitted values
int.hat2 <- ranef(fit2)[[1]][[1]] # Predicted intercepts
res.hat2 <- residuals(fit2) # Estimated residuals

qqnorm(int.hat2, main="Random Intercepts"); qqline(int.hat2)

qqnorm(res.hat2, main="Residuals"); qqline(res.hat2)
plot(y.hat2, res.hat2, xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty=2)


##########################################
#Ohio children data; GEE
library(gee)
library(MASS)
library(geepack)
library(doBy)
data(ohio)
help(ohio)
head(ohio)
tail(ohio)
str(ohio)

###############################
fit.exch <- geeglm(resp~age+smoke, family=binomial(link="logit"),
data=ohio, id=id, corstr = "exchangeable", std.err="san.se")

summary(fit.exch)

Call:
geeglm(formula = resp ~ age + smoke, family = binomial(link = "logit"), 
    data = ohio, id = id, corstr = "exchangeable", std.err = "san.se")

 Coefficients:
            Estimate  Std.err    Wald Pr(>|W|)    
(Intercept) -1.88043  0.11389 272.597  < 2e-16 ***
age         -0.11338  0.04386   6.684  0.00973 ** 
smoke        0.26508  0.17775   2.224  0.13588    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)   0.9985  0.1116

Correlation: Structure = exchangeable  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha   0.3543 0.06244
Number of clusters:   537   Maximum cluster size: 4 

> attributes(summary(fit.exch))
$names
 [1] "call"           "terms"          "family"         "contrasts"     
 [5] "deviance.resid" "coefficients"   "aliased"        "dispersion"    
 [9] "df"             "cov.unscaled"   "cov.scaled"     "corr"          
[13] "corstr"         "scale.fix"      "cor.link"       "clusz"         
[17] "error"          "geese"         

$class
[1] "summary.geeglm"

> summary(fit.exch)$cov.un
         [,1]     [,2]      [,3]
[1,]  0.01297 0.001332 -0.011960
[2,]  0.00133 0.001923  0.000124
[3,] -0.01196 0.000124  0.031594

> summary(fit.exch)$cov.sca
         [,1]     [,2]      [,3]
[1,]  0.01297 0.001332 -0.011960
[2,]  0.00133 0.001923  0.000124
[3,] -0.01196 0.000124  0.031594

########################
fit.unstr <- geeglm(resp~age+smoke, family=binomial(link="logit"),
data=ohio, id=id, corstr = "unstructured", std.err="san.se")

summary(fit.unstr)

Call:
geeglm(formula = resp ~ age + smoke, family = binomial(link = "logit"), 
    data = ohio, id = id, corstr = "unstructured", std.err = "san.se")

 Coefficients:
            Estimate Std.err   Wald Pr(>|W|)    
(Intercept)  -1.8886  0.1140 274.64   <2e-16 ***
age          -0.1149  0.0442   6.75   0.0094 ** 
smoke         0.2535  0.1782   2.02   0.1548    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)     1.01   0.115

Correlation: Structure = unstructured  Link = identity 

Estimated Correlation Parameters:
          Estimate Std.err
alpha.1:2    0.350  0.0732
alpha.1:3    0.308  0.0711
alpha.1:4    0.303  0.0710
alpha.2:3    0.470  0.0864
alpha.2:4    0.319  0.0736
alpha.3:4    0.376  0.0788
Number of clusters:   537   Maximum cluster size: 4 


attributes(summary(fit.unstr))

$names
 [1] "call"           "terms"          "family"         "contrasts"     
 [5] "deviance.resid" "coefficients"   "aliased"        "dispersion"    
 [9] "df"             "cov.unscaled"   "cov.scaled"     "corr"          
[13] "corstr"         "scale.fix"      "cor.link"       "clusz"         
[17] "error"          "geese"         

$class
[1] "summary.geeglm"

summary(fit.unstr)$cov.un
         [,1]     [,2]      [,3]
[1,]  0.01299 0.001343 -0.011956
[2,]  0.00134 0.001957  0.000152
[3,] -0.01196 0.000152  0.031750

> summary(fit.unstr)$cov.sc
         [,1]     [,2]      [,3]
[1,]  0.01299 0.001343 -0.011956
[2,]  0.00134 0.001957  0.000152
[3,] -0.01196 0.000152  0.031750
#########################################

# Treat time (age) as categorical
fit <- geeglm(resp~factor(age)+smoke, family=binomial(link="logit"),
   data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
summary(fit)

Call:
geeglm(formula = resp ~ factor(age) + smoke, family = binomial(link = "logit"), 
    data = ohio, id = id, corstr = "exchangeable", std.err = "san.se")

 Coefficients:
              Estimate Std.err   Wald Pr(>|W|)    
(Intercept)    -1.7434  0.1374 161.00   <2e-16 ***
factor(age)-1   0.0540  0.1323   0.17     0.68    
factor(age)0   -0.0278  0.1388   0.04     0.84    
factor(age)1   -0.3755  0.1467   6.55     0.01 *  
smoke           0.2712  0.1781   2.32     0.13    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)        1   0.115

Correlation: Structure = exchangeable  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha    0.354  0.0636
Number of clusters:   537   Maximum cluster size: 4 

######################
# Test the effect of smoke using anova()
fit1 <- geeglm(resp~factor(age)+smoke, family=binomial(link="logit"),
  data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
fit2 <- geeglm(resp~factor(age), family=binomial(link="logit"),
   data=ohio, id=id, corstr = "exchangeable", std.err="san.se")

anova(fit1, fit2)

Analysis of 'Wald statistic' Table

Model 1 resp ~ factor(age) + smoke 
Model 2 resp ~ factor(age)
  Df   X2 P(>|Chi|)
1  1 2.32      0.13

###
> summary(fit1)

Call:
geeglm(formula = resp ~ factor(age) + smoke, family = binomial(link = "logit"), 
    data = ohio, id = id, corstr = "exchangeable", std.err = "san.se")

 Coefficients:
              Estimate Std.err   Wald Pr(>|W|)    
(Intercept)    -1.7434  0.1374 161.00   <2e-16 ***
factor(age)-1   0.0540  0.1323   0.17     0.68    
factor(age)0   -0.0278  0.1388   0.04     0.84    
factor(age)1   -0.3755  0.1467   6.55     0.01 *  
smoke           0.2712  0.1781   2.32     0.13    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)        1   0.115

Correlation: Structure = exchangeable  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha    0.354  0.0636
Number of clusters:   537   Maximum cluster size: 4 


### Individual Wald test and confidence interval for each parameter
est <- esticon(fit, diag(5))

> est
  beta0 Estimate Std.Error X2.value DF Pr(>|X^2|)   Lower  Upper
1     0  -1.7434     0.137  160.995  1     0.0000 -2.0127 -1.474
2     0   0.0540     0.132    0.167  1     0.6831 -0.2053  0.313
3     0  -0.0278     0.139    0.040  1     0.8415 -0.2998  0.244
4     0  -0.3755     0.147    6.552  1     0.0105 -0.6631 -0.088
5     0   0.2712     0.178    2.319  1     0.1278 -0.0778  0.620

########
# Odds ratio and confidence intervals
OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
rownames(OR.CI) <- names(coef(fit))
colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
OR.CI

                 OR Lower OR Upper OR
(Intercept)   0.175    0.134    0.229
factor(age)-1 1.055    0.814    1.368
factor(age)0  0.973    0.741    1.277
factor(age)1  0.687    0.515    0.916
smoke         1.312    0.925    1.859

#########
# Odds ratio of wheezing for a 9-year old with a mother who smoked
# during the first year of the study compared to an 8-year old with a
# mother who did not smoke during the first year of the study
# That is estimate, [smoke+factor(age)0] - [factor(age)-1]

esticon(fit, c(0,-1,1,0,1))
  beta0 Estimate Std.Error X2.value DF Pr(>|X^2|)  Lower Upper
1     0    0.189     0.215    0.773  1      0.379 -0.233 0.612

exp(.Last.value$Estimate)
[1] 1.21
# 9-year old with mother who smoked is at greater risk of wheezing


####
# Jointly test effects using esticon()
fit <- geeglm(resp~factor(age)*smoke, family=binomial(link="logit"),
data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
summary(fit)
L = cbind(matrix(0, nrow=3, ncol=5), diag(3))
L
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
[1,]    0    0    0    0    0    1    0    0
[2,]    0    0    0    0    0    0    1    0
[3,]    0    0    0    0    0    0    0    1

esticon(fit, L, joint.test=TRUE)
  X2.stat DF Pr(>|X^2|)
1    1.97  3      0.578

esticon(fit, L, joint.test=FALSE)
  beta0 Estimate Std.Error X2.value DF Pr(>|X^2|)  Lower Upper
1     0    0.370     0.271    1.862  1      0.172 -0.161 0.901
2     0    0.281     0.284    0.980  1      0.322 -0.275 0.837
3     0    0.270     0.299    0.814  1      0.367 -0.316 0.855


#################
# Could also use anova()
fit1 <- geeglm(resp~factor(age)*smoke, family=binomial(link="logit"),
  data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
fit2 <- geeglm(resp~factor(age)+smoke, family=binomial(link="logit"),
  data=ohio, id=id, corstr = "exchangeable", std.err="san.se")

anova(fit1, fit2)

Analysis of 'Wald statistic' Table

Model 1 resp ~ factor(age) * smoke 
Model 2 resp ~ factor(age) + smoke
  Df   X2 P(>|Chi|)
1  3 1.97      0.58

################################################################
#########Ohio children data, GLMM
library(lme4)
fitQ10 <- glmer(resp ~ age + smoke + (1|id), family=binomial, data=ohio)
summary(fitQ10)

Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: resp ~ age + smoke + (1 | id)
   Data: ohio

     AIC      BIC   logLik deviance df.resid 
  1597.9   1620.6   -794.9   1589.9     2144 

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.403 -0.180 -0.158 -0.132  2.518 

Random effects:
 Groups Name        Variance Std.Dev.
 id     (Intercept) 5.49     2.34    
Number of obs: 2148, groups:  id, 537

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -3.374      0.275  -12.27   <2e-16 ***
age           -0.177      0.068   -2.60   0.0093 ** 
smoke          0.415      0.287    1.44   0.1485    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
      (Intr) age   
age    0.227       
smoke -0.419 -0.010

