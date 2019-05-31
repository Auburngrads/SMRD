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

## ---- eval=FALSE---------------------------------------------------------
#  comptime.mlest.out <- groupm.mleprobplot(comptime.ld,
#                                           distribution ="Lognormal",
#                                           group.var = 1,
#                                           relationship = "linear")
#  
#  comptime.mlest.out2 <- groupm.mleprobplot(comptime.ld,
#                                            distribution ="Normal",
#                                            group.var = 1,
#                                            relationship = "linear")
#  
#  SMRD2:::resid.vs.order(comptime.mlest.out)
#  
#  SMRD2:::resid.vs.fit(comptime.mlest.out)
#  
#  SMRD2:::resid.vs.explan(comptime.mlest.out)
#  
#  SMRD2:::resid.probplot(comptime.mlest.out)
#  
#  plot(comptime.mlest.out,
#       density.at = c(1, 3, 5))
#  
#  plot(comptime.mlest.out,
#       density.at = c(1, 3, 5),
#       response.on.yaxis = F)
#  
#  #or more simply as
#  
#  plot(comptime.mlest.out)
#  
#  quantiles(comptime.mlest.out,new.data=5)

## ------------------------------------------------------------------------
Snubber.ld <- frame.to.ld(snubber, 
                          response.column = "cycles", 
                          censor.column = "event",
                          time.units = "Cycles",
                          case.weight.column = "count",
                          x.columns = "design")

event.plot(Snubber.ld)
summary(Snubber.ld)

Snubber.groupi.nor.out <-  groupi.mleprobplot(Snubber.ld,"normal")

tmpxx <- groupi.contour(Snubber.ld,
                        "Weibull",
                        the.quantile = 0.1)

tmpxx <- groupi.contour(Snubber.ld,
                        "lognormal",
                        the.quantile = 0.1)

tmpxx <- groupi.contour(Snubber.ld,
                        "lognormal")

tmpxx <- groupi.contour(Snubber.ld,
                        "normal")

tmpxx <- groupi.contour(Snubber.ld,
                        "normal",
                        the.quantile = 0.1)

multiple.profile.plot(tmpxx, which = "x")
multiple.profile.plot(tmpxx, which = "y")

summary(Snubber.groupi.nor.out)

Snubber.groupm.nor.out <-  groupm.mleprobplot(Snubber.ld,
                                              distribution = "Normal",
                                              relationship ="class")

quantiles(Snubber.groupm.nor.out,
          new.data = "Old")

quantiles(Snubber.groupm.nor.out,
          new.data = "New")

plot(Snubber.groupm.nor.out)

## ------------------------------------------------------------------------
PartA.ld <- frame.to.ld(parta,
                        response.column = "kilocycles",
                        x.columns = "operator")

groupi.contour(PartA.ld,
               rel.or.conf = "",
               "Weibull",
               the.quantile = 0.10)

groupi.contour(PartA.ld,
               "Weibull",
               the.quantile = 0.10)

groupi.contour(PartA.ld,
               rel.or.conf = "",
               "Weibull",
               the.quantile = 0.005)

groupi.contour(PartA.ld,
               "Weibull",
               the.quantile = 0.005)

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

## ------------------------------------------------------------------------
ZelenCap.groupm.out1 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "Weibull")

SMRD2:::resid.vs.order(ZelenCap.groupm.out1)

SMRD2:::resid.vs.fit(ZelenCap.groupm.out1)

SMRD2:::resid.vs.explan(ZelenCap.groupm.out1,
                        x.to.plot = 1)

SMRD2:::resid.vs.explan(ZelenCap.groupm.out1,
                        x.to.plot = 2)

SMRD2:::resid.probplot(ZelenCap.groupm.out1)

## ------------------------------------------------------------------------
ZelenCap.groupm.out2 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "Lognormal",
                     formula= Location ~ g(celsius),
                     relationship = c("arrhenius", "log"))


## ------------------------------------------------------------------------
ZelenCap.groupm.out3 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "Lognormal",
                     formula= Location ~ g(volts),
                     relationship = c("arrhenius", "log"))

## ------------------------------------------------------------------------
ZelenCap.groupm.out4 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "normal",
                     formula = Location ~ g(celsius) +  g(volts))

ZelenCap.groupm.out5 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "normal",
                     formula= Location ~ g(volts) + g(celsius))

## ------------------------------------------------------------------------
ZelenCap.groupm.out6 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "normal",
                     formula = Location ~ celsius + volts + celsius:volts )

## ------------------------------------------------------------------------
ZelenCap.groupm.out7 <-
  groupm.mleprobplot(ZelenCap.ld,
                     distribution = "Lognormal",
                     relationship = c("arrhenius","log"))

## ------------------------------------------------------------------------
ZelenCap.groupm.out3 <- 
  groupm.mleprobplot(ZelenCap.ld, 
                     distribution = "Lognormal", 
                     relationship = c("linear", "linear"), 
                     formula = Location ~ g(volts) + g(celsius) + g(volts):g(celsius))

## ---- eval=FALSE---------------------------------------------------------
#  #make a proper dataframe for new data (used below)
#  frame.new.data("165;188",ZelenCap.groupm.out3[[1]])
#  
#  #temperature and voltage need be in the right order, semicolon separated
#  quantiles(ZelenCap.groupm.out3,
#            new.data = "165;188")
#  
#  failure.probabilities(ZelenCap.groupm.out3,
#                        new.data = "165;188")
#  
#  SMRD2:::resid.vs.explan.multiple(ZelenCap.groupm.out3)
#  residual.plots(ZelenCap.groupm.out3)

## ------------------------------------------------------------------------
superalloy.ld <- frame.to.ld(superalloy,
                             response.column = 1,
                             censor.column = 2,
                             x.columns = c(4,5,6),
                             data.title = "Nelson's Super Alloy Fatigue Data",
                             time.units = "Kilocycles")

summary(superalloy.ld)

censored.data.plot(superalloy.ld)

## ---- eval=FALSE---------------------------------------------------------
#  gmlest(superalloy.ld,
#         dist = "Weibull",
#         explan.vars = list(mu.relat = c(2,3),
#                            sigma.relat = c(2)))

## ---- eval=FALSE---------------------------------------------------------
#  frame.new.data("165;150", ZelenCap.groupm.out3[[1]])

