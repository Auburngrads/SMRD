## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
HeatExchanger.ld <- frame.to.ld(heatexchanger,
                                response.column = c(1,2), 
                                censor.column = 3,
                                case.weight.column = 4,
                                time.units = "Years")

summary(HeatExchanger.ld)

event.plot(HeatExchanger.ld)

plot(HeatExchanger.ld, 
     band.type = "Pointwise",
     ylim = c(0,.2))

plot(HeatExchanger.ld,
     band.type = "Simultaneous",
     ylim = c(0,.2))

cdfest(HeatExchanger.ld)

plot(HeatExchanger.ld,
     dist = "Weibull",
     band.type = "none")

plot(HeatExchanger.ld,
     band.type = "none")

## ------------------------------------------------------------------------
lfp1370.ld <- frame.to.ld(lfp1370,
                          response.column = 1, 
                          censor.column = 2,
                          case.weight.column = 3,
                          time.units = "Hours")

event.plot(lfp1370.ld)
plot(lfp1370.ld)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1,
                                censor.column = 3,
                                time.units = "Kilometers")


event.plot(ShockAbsorber.ld)
summary(ShockAbsorber.ld)


plot(ShockAbsorber.ld,
     band.type = "Pointwise",
     ylim = c(0.0,.99))

plot(ShockAbsorber.ld,
     band.type = "Simultaneous",
     ylim = c(0.0,.99))

plot(ShockAbsorber.ld,
     band.type = "Simultaneous", 
     plot.censored.ticks = "top")

## ------------------------------------------------------------------------
Fan.ld <- frame.to.ld(fan,
                      response.column = 1, 
                      censor.column = 2, 
                      case.weight.column = 3,
                      time.units = "Hours")

event.plot(Fan.ld)

summary(Fan.ld)

plot(Fan.ld, 
     plot.censored.ticks = "top")

plot(Fan.ld, 
     plot.censored.ticks = "top",
     distribution = "exponential",
     shape = c(15))

