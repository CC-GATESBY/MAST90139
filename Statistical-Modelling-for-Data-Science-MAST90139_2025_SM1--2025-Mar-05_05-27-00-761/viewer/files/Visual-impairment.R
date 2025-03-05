##This script is used to analyse the Visual Impairment data.
library(gee)
library(geepack)
VI.dat <- read.csv("C:/Users/qguoqi/OneDrive - The University of Melbourne/My Documents/MAST90084/Visual-impairment.csv")
#VI.dat <- read.csv("D:/MAST90139/Visual-impairment.csv")
is.data.frame(VI.dat)
VI.dat
is.factor(VI.dat$Age)
is.factor(VI.dat$Race)

######################################Usage of gee()
VI1<- gee(cbind(Yes, No) ~ Age + Race, id = ID,
           data = VI.dat, family = binomial, corstr = "unstructured")

VI.dat$Total = VI.dat$Yes+VI.dat$No

VI1a<- gee(Yes/Total ~ Age + Race, id = ID, weight=Total,
           data = VI.dat, family = binomial, corstr = "unstructured")
##gee does not use Yes/Total as the response, and does not use weights argument
summary(VI1)

VI1e<- gee(cbind(Yes, No) ~ Age + Race, id = ID,
           data = VI.dat, family = binomial, corstr = "exchangeable") #does not work
summary(VI1e)

total=VI.dat$Yes+VI.dat$No
pearson.res=(VI.dat$Yes-total*VI1$fitted)/sqrt(total*(VI1$fitted)*(1-VI1$fitted))
> sum(pearson.res^2)/11    #11=N-p
[1] 0.5708722
> VI1$scale
[1] 0.5708722
pearson.res1=(VI.dat$Yes/total-VI1$fitted)/sqrt((VI1$fitted)*(1-VI1$fitted)/total)
#same as pearson.res

pearson.res1e=(VI.dat$Yes-total*VI1e$fitted)/sqrt(total*(VI1e$fitted)*(1-VI1e$fitted))
sum(pearson.res1e^2)/11
[1] 0.5708722 #estimate of \phi based on Pearson residuals

> VI1e$scale
[1] 0.5708722

(VI1e$scale*3)^(-1)*sum(pearson.res1e[2*(1:8)-1]*pearson.res1e[2*(1:8)])
[1] 0.8861748
> VI1e$working.correlation
          [,1]      [,2]
[1,] 1.0000000 0.8861748
[2,] 0.8861748 1.0000000


attributes(VI1)

> VI1$fitted   #These are fitted probabilities
 [1] 0.02724955 0.02724955 0.04323931 0.04323931 0.06078628 0.06078628
 [7] 0.16766361 0.16766361 0.03822630 0.03822630 0.06025811 0.06025811
[13] 0.08410439 0.08410439 0.22227774 0.22227774

> VI1$residual  #These results are not correct, because they are the difference 
                #between the numbers of "success" and the fitted probabilities.
 [1]  14.97275  18.97275  23.95676  24.95676  41.93921  47.93921 138.83234
 [8] 145.83234  28.96177  30.96177  37.93974  36.93974  49.91590  48.91590
[15]  84.77772  92.77772

> VI.dat$Yes-VI1$fitted
 [1]  14.97275  18.97275  23.95676  24.95676  41.93921  47.93921 138.83234
 [8] 145.83234  28.96177  30.96177  37.93974  36.93974  49.91590  48.91590
[15]  84.77772  92.77772


######################################Usage of geelm()
VI2 <- geeglm(cbind(Yes, No)~Age + Race, family=binomial(link="logit"),
data=VI.dat, id=ID, corstr = "unstructured", std.err="san.se")  #does not work

summary(VI2)

VI3 <- geeglm(cbind(Yes, No)~Age + Race, family=binomial(link="logit"),
data=VI.dat, id=ID, corstr = "exchangeable", std.err="san.se")

VI3a <- geeglm(Yes/Total~Age + Race, family=binomial(link="logit"), 
weights=Total, data=VI.dat, id=ID, corstr = "exchangeable", std.err="san.se")
#VI3a and VI3 are the same

summary(VI3)

pearson.res3=(VI.dat$Yes/total-VI3$fitted)/sqrt((VI3$fitted)*(1-VI3$fitted)/total)
phi.est=sum(pearson.res3^2)/11   #=0.5709

> sum((summary(VI3)$deviance.resid)^2)/11
[1] 0.577  #very close to 0.5709

> VI3$geese$gamma
(Intercept) 
   0.000604 

(phi.est*3)^(-1)*sum(pearson.res3[2*(1:8)-1]*pearson.res3[2*(1:8)])
[1] 0.8862  #estimated alpha

> (0.577*3)^(-1)*sum(summary(VI3)$devi[2*(1:8)-1]*summary(VI3)$devi[2*(1:8)])
[1] 0.872

> VI3$geese$alpha
 alpha 
0.4415 #approximately half of 0.8862

#####################################
VIind=glm(Yes/(Yes+No)~Age + Race, family=binomial(link="logit"),data=VI.dat, weight=Yes+No)

VIind1=glm(cbind(Yes, No)~Age + Race, family=binomial(link="logit"),data=VI.dat)