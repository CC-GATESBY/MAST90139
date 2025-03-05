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

BP.v <- rep(1:4, 4)
CHOL.v <- rep(1:4, rep(4, 4))

fit.5 <- glm(Y/N ~ CHOL.v + BP.v, weights = N, family = "binomial")
summary(fit.5)
attributes(summary(fit.5))
summary(fit.5)$cov.unscaled
summary(fit.5)$cov.scaled

anova(fit.5, test="Chi")

anova(fit.5, fit.4, test="Chi")

PearD <- sum(resid(fit.5, type="pearson")^2)
1-pchisq(PearD,13)

matrix(resid(fit.5, type="deviance"), 4, 4, byrow=T)

matrix(resid(fit.5, type="pearson"), 4, 4, byrow=T)
diff=resid(fit.5, type="pearson")-resid(fit.5, type="deviance")
matrix(diff, 4, 4, byrow=T)



fit.6 <- glm(Y/N ~ (CHOL.v + BP.v)^2, weights = N, family = "binomial")
summary(fit.6)
anova(fit.6, test="Chi")

sqrt(c(1,4,3)%*%summary(fit.5)$cov.un%*%matrix(c(1,4,3),3,1))



