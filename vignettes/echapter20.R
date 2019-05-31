## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
AdhesiveBond.Weibull.altpv <-
  get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
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
AdhesiveBond0.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(100,100,100),
                           censor.times = c(243,243,243))

AdhesiveBond1.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(78,98,120),
                           number.of.units = c(155,60,84),
                           censor.times = c(183,183,183))

AdhesiveBond2.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(212,88,0),
                           censor.times = c(243,243,243))

AdhesiveBond3.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(150,60,90),
                           censor.times = c(243,243,243))

## optimum plan

AdhesiveBond4.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(95,120),
                           number.of.units = c(212,88),
                           censor.times = c(183,183))

print(AdhesiveBond1.altplan)

## ------------------------------------------------------------------------
AdhesiveBond2.frame <- data.frame(DegreesC = c(80,100,120),
                                  SampleSize = c(150,60,90),
                                  CensorTimes = c(243,243,243))

AdhesiveBond2.altplan <- 
  get.alt.test.plan.frame(AdhesiveBond2.frame,
                          levels.columns = "DegreesC", 
                          censor.column = "CensorTimes",
                          allocation.column = "SampleSize",
                          describe.string = "AdhesiveBond Test Plan")

print(AdhesiveBond2.altplan)

## ------------------------------------------------------------------------
plot(AdhesiveBond1.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

plot(AdhesiveBond2.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

plot(AdhesiveBond3.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

## ------------------------------------------------------------------------
ALT.vcv(AdhesiveBond1.altplan,
        ALT.plan.values = AdhesiveBond.Weibull.altpv)

evaluate(AdhesiveBond1.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         quantile.of.interest = 0.5,
         use.condition = 50)

names(AdhesiveBond1.altplan)
names(AdhesiveBond.Weibull.altpv)

## ------------------------------------------------------------------------
simulate(AdhesiveBond1.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         use.condition = 50)

unfold(AdhesiveBond1.altplan, 
       AdhesiveBond.Weibull.altpv, 
       use.condition = 50)

evaluate(AdhesiveBond1.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.5)

## ------------------------------------------------------------------------
simulate(AdhesiveBond2.altplan, 
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         show.detail.on = 5)

simulate(AdhesiveBond2.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         number.sim = 100,
         use.condition = 50)

evaluate(AdhesiveBond2.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.1)

AdhesiveBond3.sim.out <- 
  simulate(AdhesiveBond3.altplan,
           ALT.plan.values = AdhesiveBond.Weibull.altpv,
           use.condition = 50)

ALT.plot.time.v.x(AdhesiveBond3.sim.out,
                  x.of.interest = c(45))

ALT.plot.FracFail.v.Time(AdhesiveBond3.sim.out,
                         x.of.interest = c(45))

evaluate(AdhesiveBond3.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.1)

SMRD2:::plot.vpm.vs.size(vpm.vec = c(100, 150, 200), 
                         size.vec = c(1, 2, 3))

SMRD2:::plot.volt.vs.size(volts.vec = c(100, 200, 300), 
                          size.vec = c(1, 2, 3))

## ------------------------------------------------------------------------
DeviceA.altpv <- 
  get.alt.plan.values.from.slope.and.point(distribution = "Lognormal",
                                           slope = 0.63,
                                           relationship = "Arrhenius",
                                           time.units = "Hours",
                                           censor.time = 5000,
                                           probs = 0.5469, 
                                           accelvar = 60,
                                           sigma = 0.98,
                                           use.conditions = 10)

print(DeviceA.altpv)

DeviceA1.altplan <- 
  get.alt.test.plan.direct(accel.variable.levels = c(10,40,60,80),
                           number.of.units = c(30,100,20,15),
                           censor.time =  c(5000,5000,5000,5000))

print(DeviceA1.altplan)

simulate(DeviceA1.altplan,
         ALT.plan.values = DeviceA.altpv, 
         nsim = 100,
         use.condition = 10)

DeviceA2.altplan <- 
  get.alt.test.plan.direct(accel.variable.levels = c(10,40,50,60,70,80),
                           number.of.units = c(30,27,27,27,27,27),
                           censor.time = c(5000,5000,5000,5000,5000,5000))

print(DeviceA2.altplan)

simulate(DeviceA2.altplan,
         ALT.plan.values = DeviceA.altpv, 
         nsim = 100, 
         use.condition = 10)

## ---- eval=FALSE---------------------------------------------------------
#  ## generate ALT plans
#  
#  hold.altplan("Weibull",
#               a = -4,
#               b1 = -10,
#               perc = 0.1,
#               iopta = 3,
#               iopts = 2,
#               pifix = 0)

