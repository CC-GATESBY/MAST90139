Caes.dat <- read.csv("D:/MAST90139/Example6-1Caes.csv")
is.data.frame(Caes.dat)
library(nnet)
Caes1=multinom(as.matrix(Caes.dat[,2:4])~NoPlan+Antib+RiskF,data=Caes.dat)
Caes1
summary(Caes1)
step(Caes1)
attributes(Caes1)
$names
 [1] "n"             "nunits"        "nconn"         "conn"         
 [5] "nsunits"       "decay"         "entropy"       "softmax"      
 [9] "censored"      "value"         "wts"           "convergence"  
[13] "fitted.values" "residuals"     "call"          "terms"        
[17] "weights"       "deviance"      "rank"          "lab"          
[21] "coefnames"     "vcoefnames"    "xlevels"       "edf"          
[25] "AIC"          

$class
[1] "multinom" "nnet"    

Caes1$fitted
Caes1$residuals

> Caes1.grp=Caes.dat[,2:4]
> Caes1.grp[,1]=Caes.dat[,2]/Caes.dat[,1]
> Caes1.grp[,2]=Caes.dat[,3]/Caes.dat[,1]
> Caes1.grp[,3]=Caes.dat[,4]/Caes.dat[,1]
> Caes1.grp-Caes1$fitted
##This returns the residuals given by Caes1$residuals

Caes1$deviance
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="class")
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs", se.fit=TRUE)

Caes2=multinom(as.matrix(Caes.dat[,2:4])~NoPlan+Antib,data=Caes.dat)
summary(Caes2)
Caes2$deviance
