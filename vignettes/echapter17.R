## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
comptime.ld <- frame.to.ld(comptime,
                           response.column = 3, 
                           x.column = 2,
                           time.units = "Seconds", 
                           xlabel = "System.Load")

summary(comptime.ld)

censored.data.plot(comptime.ld,
                   xlab = "System Load")

## ------------------------------------------------------------------------
comptime.mlest.out <- groupm.mleprobplot(comptime.ld, 
                                         distribution ="Lognormal", 
                                         group.var = 1, 
                                         relationship = "linear")

comptime.mlest.out2 <- groupm.mleprobplot(comptime.ld, 
                                          distribution ="Normal", 
                                          group.var = 1, 
                                          relationship = "linear")

## ------------------------------------------------------------------------
Snubber.ld <- frame.to.ld(snubber, 
                          response.column = "cycles", 
                          censor.column = "event",
                          time.units = "Cycles",
                          case.weight.column = "count",
                          x.columns = "design")

## ------------------------------------------------------------------------
ZelenCap.ld <- frame.to.ld(zelencap,
                           response.column = 1,
                           censor.column = 2,
                           case.weight.column = 3,
                           x.columns = c(4, 5),
                           time.units = "Hours")

## ------------------------------------------------------------------------
ZelenCap.groupi.Weibull.out <- 
  groupi.mleprobplot(ZelenCap.ld, 
                     distribution = "Weibull", 
                     group.var = c(1, 2))

summary(ZelenCap.groupi.Weibull.out)

names(xmat(ZelenCap.ld))

