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

