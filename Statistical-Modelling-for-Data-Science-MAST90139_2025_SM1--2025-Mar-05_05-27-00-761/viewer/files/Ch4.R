voting<-read.csv("C:/Users/qguoqi/OneDrive - The University of Melbourne/My Documents/MAST90139/data/voting.csv")
plot(voting$views, voting$reagan/voting$total, 
  xlab="Political Views", ylab="Proportion for Reagan")

vote.dat<-read.csv("C:/Users/qguoqi/OneDrive - The University of Melbourne/My Documents/MAST90139/data/voting.csv")
vote.1 <- glm(reagan/total ~ views, family=binomial, weight=total,  data=vote.dat)
summary(vote.1)
anova(vote.1, test = "Chi")

vote.2 <- glm(reagan/total ~ views, family=quasibinomial, weight=total,  data=vote.dat)
summary(vote.2)

> summary(vote.2)$cov.scale
            (Intercept)        views
(Intercept)  0.09034790 -0.019658069
views       -0.01965807  0.004624032
> summary(vote.2)$cov.unscale
            (Intercept)        views
(Intercept)  0.07157781 -0.015574037
views       -0.01557404  0.003663373
> summary(vote.2)$cov.unscale[1,1]
[1] 0.07157781
> summary(vote.2)$cov.unscale[1,1]*1.262233
[1] 0.09034787
> sqrt(summary(vote.2)$cov.scale[1,1])
[1] 0.3005793
