## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
LocomotiveControl.ld <- frame.to.ld(locomotivecontrol,
                                    response.column = 1, 
                                    censor.column = 2,
                                    case.weight.column = 3,
                                    time.units = "kMiles")

LocomotiveControl.gamma.gmle.out <- Gamma.mle(LocomotiveControl.ld)

gmleprobplot(LocomotiveControl.ld,
             distribution = "gamma", 
             xlim = c(10,199),
             ylim = c(.0011,.991))

## ------------------------------------------------------------------------
## gamma dist examples
lzbearing.ld <- frame.to.ld(lzbearing, response.column = 1)

bear.gamma.gmle.out <- Gamma.mle(lzbearing.ld)

summary(bear.gamma.gmle.out)

gmleprobplot(lzbearing.ld,
             distribution = "gamma",
             xlim = c(10,199),
             ylim = c(.0011,.991),
             compare = c("Lognormal","Weibull"))

legend(x = c(3.6,5.2),
       y = c(-1.6,-2.7), 
       c("Gamma","Lognormal","Weibull","95% pointwise confidence intervals"),
       lty = c(1,3,4,2),
       bty = "n",
       cex = 1.2)

## ------------------------------------------------------------------------
bkfatigue10.ld <- frame.to.ld(bkfatigue10, 
                              response.column = 1,
                              time.units = "Kilocycles")

summary(bkfatigue10.ld)

bkfatigue10.gamma.gmle.out <- Gamma.mle(bkfatigue10.ld)

gmleprobplot(bkfatigue10.ld,
             distribution = "gamma",
             compare = c("Lognormal"))

gmleprobplot(bkfatigue10.ld,
             distribution = "Lognormal",
             compare = c("gamma"))

## ------------------------------------------------------------------------
at7987.ld <- frame.to.ld(at7987, 
                         response.column = 1,
                         censor.column = 2, 
                         case.weight.column = 3,
                         time.units = "Kilocycles")

summary(at7987.ld)

at7987.gamma.gmle.out <- Gamma.mle(at7987.ld)

gmleprobplot(at7987.ld,
             distribution = "Gamma",
             compare = c("Lognormal"))

