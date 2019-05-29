## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
AdhesiveBond.Weibull.altpv <- get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
                                                                       relationship = "Arrhenius", 
                                                                       accelvar.units = c("DegreesC"),
                                                                       time.units = "Days", 
                                                                       censor.time = 183,
                                                                       probs = c(.001), 
                                                                       accelvar = c(50),
                                                                       slope = 0.726, 
                                                                       beta = 1.667)

print(AdhesiveBond.Weibull.altpv)

## ------------------------------------------------------------------------
AdhesiveBond0.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                                                  number.of.units = c(100,100,100),
                                                  censor.times = c(243,243,243))

AdhesiveBond1.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(78,98,120),
                                                  number.of.units = c(155,60,84),
                                                  censor.times = c(183,183,183))

AdhesiveBond2.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                                                  number.of.units = c(212,88,0),
                                                  censor.times = c(243,243,243))

AdhesiveBond3.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                                                  number.of.units = c(150,60,90),
                                                  censor.times = c(243,243,243))

## optimum plan

AdhesiveBond4.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(95,120),
                                                  number.of.units = c(212,88),
                                                  censor.times = c(183,183))

print(AdhesiveBond1.altplan)

