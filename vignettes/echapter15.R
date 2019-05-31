## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
component.effect(parallel = TRUE)

parallel.effect()

parallel.dep.effect()

series.dep.effect()

## ------------------------------------------------------------------------
DeviceG.ld <- frame.to.ld(deviceg,
                          response.column = 1, 
                          failure.mode.column =  2)

summary(DeviceG.ld)

## ------------------------------------------------------------------------
event.plot(DeviceG.ld)

## ------------------------------------------------------------------------
plot(DeviceG.ld, distribution = c("weibull", "lognormal"))

## ------------------------------------------------------------------------
par(mfrow = c(1,2))
mfmi.mleprobplot(DeviceG.ld, distribution = "weibull") 
mfmc.mleprobplot(DeviceG.ld, distribution = "lognormal") 
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
mfmc.mleprobplot(DeviceG.ld, 
                 distribution = "Weibull") 

mfmc.mleprobplot(DeviceG.ld, 
                 distribution = "Weibull", 
                 band.type = "none")

tmp <- mfmc.mleprobplot(DeviceG.ld, 
                       distribution = "Weibull", 
                       band.type = "none", 
                       show.individual = F, 
                       ylim = c(0.1, .99))

failure.probabilities(tmp)
quantiles(tmp)

## ------------------------------------------------------------------------
tmpx <- mfmc.mleprobplot(DeviceG.ld, 
                         distribution = "Weibull", 
                         distribution.vec = c("Weibull","Lognormal"))

failure.probabilities(tmpx)
quantiles(tmpx)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1,
                                failure.mode.column = 2,
                                censor.column = 3, 
                                time.units = "Kilometers")
summary(ShockAbsorber.ld)
event.plot(ShockAbsorber.ld)

## ------------------------------------------------------------------------
mleprobplot(ShockAbsorber.ld, 
            distribution = "Weibull")

mfmi.mleprobplot(ShockAbsorber.ld, 
                 distribution = "Weibull")

tmpx <- mfmc.mleprobplot(ShockAbsorber.ld, 
                         distribution = "Weibull")

failure.probabilities(tmpx)
quantiles(tmpx)

## ------------------------------------------------------------------------
ShockAbsorber.mfld <- mfm.to.ld(ShockAbsorber.ld)

multiple.mleprobplot(ShockAbsorber.mfld,
                     data.ld.name="xx",
                     xlab="yy",
                     distribution="Weibull")

mleprobplot(ShockAbsorber.Mode1.ld, 
            distribution = "Weibull")

mleprobplot(ShockAbsorber.Mode2.ld,
            distribution = "Weibull")

get.time.vector(ShockAbsorber.Mode2.ld)

## ------------------------------------------------------------------------
ConnectionStrength.ld <- 
  frame.to.ld(connectionstrength,
              response.column = 1,
              failure.mode.column = 2,
              case.weight.column = 3)

summary(ConnectionStrength.ld )
event.plot(ConnectionStrength.ld)

mfm.to.ld(ConnectionStrength.ld)

mleprobplot(ConnectionStrength.Bond.ld , 
            distribution = "normal")

mlest(ConnectionStrength.Bond.ld , 
      distribution = "normal")

## ---- eval=FALSE---------------------------------------------------------
#  gmlest(ConnectionStrength.Bond.ld ,
#         distribution = "normal")

## ------------------------------------------------------------------------
mfmi.mleprobplot(ConnectionStrength.ld,
                 distribution = "Normal")

tpmx <- mfmc.mleprobplot(ConnectionStrength.ld,
                         distribution = "Normal")

failure.probabilities(tmpx)
quantiles(tmpx)

