## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
BearingCage.ld <- frame.to.ld(bearingcage,
                              response.column = 1, 
                              censor.column = 2,
                              case.weight.column = 3)

## ------------------------------------------------------------------------
BearingCage.mlest.weib <- mlest(BearingCage.ld,
                                dist = "Weibull")

CondProbInterval2(BearingCage.mlest.weib,
                  age = 50,
                  tL = 50,
                  tU = 350)

CondProbInterval2(BearingCage.mlest.weib,
                  age = 150,
                  tL = 150,
                  tU = 450)

## ------------------------------------------------------------------------
PredictTable(BearingCage.mlest.weib,
             FtimeStart = 0, 
             FtimeEnd = 300)

## ------------------------------------------------------------------------
CondProbInterval(mu = 3.7393, 
                 sigma = 0.7639,
                 distribution = "lognormal",
                 age = 1,
                 tL = 1,
                 tU = 36)

## ------------------------------------------------------------------------
DeviceN.ld <- frame.to.ld(devicen,
                          response.column = "months", 
                          censor.column = "event",
                          case.weight.column = "counts")

## ---- eval=FALSE---------------------------------------------------------
#  DeviceN.mlest.lnorm <- mlest(DeviceN.ld,
#                               dist = "Lognormal")
#  
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 1,
#                    tL = 1,
#                    tU = 36)
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 2,
#                    tL = 2,
#                    tU = 36)
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 1,
#                    tL = 1,
#                    tU = 2)
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 2,
#                    tL = 2,
#                    tU = 3)

## ---- eval=FALSE---------------------------------------------------------
#  DeviceN.mlest.weib <- mlest(DeviceN.ld,
#                              dist = "Weibull")
#  
#  CondProbInterval2(DeviceN.mlest.weib,
#                    age = 1,
#                    tL = 1,
#                    tU = 36)
#  
#  CondProbInterval2(DeviceN.mlest.weib,
#                    age = 2,
#                    tL = 2,
#                    tU = 36)

## ---- eval=FALSE---------------------------------------------------------
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 1,
#                    tL = 1,
#                    tU = 2)
#  
#  CondProbInterval2(DeviceN.mlest.lnorm,
#                    age = 2,
#                    tL = 2,
#                    tU = 3)
#  
#  PredictTable(DeviceN.mlest.lnorm,
#               FtimeStart = 0,
#               FtimeEnd = 1)
#  
#  PredictTable(DeviceN.mlest.lnorm,
#               FtimeStart = 1,
#               FtimeEnd = 2)
#  
#  PredictTable(DeviceN.mlest.lnorm,
#               FtimeStart = 0,
#               FtimeEnd = 36)
#  
#  PredictTable(DeviceN.mlest.lnorm,
#               FtimeStart = 0,
#               FtimeEnd = 36,
#               warranty.time = 36)
#  
#  PredictTable(DeviceN.mlest.weib,
#               FtimeStart = 1,
#               FtimeEnd = 2)
#  
#  PredictTable(DeviceN.mlest.weib,
#               FtimeStart = 0,
#               FtimeEnd = 36)

## ---- eval=FALSE---------------------------------------------------------
#  DeviceN.NoEnforce.lnor <-
#    CumulativePredictTable(DeviceN.mlest.lnorm,
#                           time.increment = 1,
#                           number.time.units.ahead = 50,
#                           warranty.time = 1000)
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.lnor,
#                             plot.what = "density")
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.lnor,
#                             plot.what="cumulative")

## ---- eval=FALSE---------------------------------------------------------
#  DeviceN.Enforce.lnor <-
#    CumulativePredictTable(DeviceN.mlest.lnorm,
#                           time.increment = 1,
#                           number.time.units.ahead = 50,
#                           warranty.time = 36)
#  
#  PlotCumulativePredictTable(DeviceN.Enforce.lnor,
#                             plot.what = "density")
#  
#  PlotCumulativePredictTable(DeviceN.Enforce.lnor,
#                             plot.what = "cumulative")

## ---- eval=FALSE---------------------------------------------------------
#  PlotCumulativePredictTable(DeviceN.NoEnforce.lnor,
#                             plot.what = "density",
#                             ylim = c(0,10000))
#  
#  PlotCumulativePredictTable(DeviceN.Enforce.lnor,
#                             plot.what = "density",
#                             add = TRUE,
#                             lty = 3,
#                             lwd = 3)
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.lnor,
#                             plot.what = "cumulative")
#  
#  PlotCumulativePredictTable(DeviceN.Enforce.lnor,
#                             plot.what = "cumulative",
#                             add = TRUE,
#                             lty = 3,
#                             lwd = 3)

## ---- eval=FALSE---------------------------------------------------------
#  DeviceN.NoEnforce.weib <-
#    CumulativePredictTable(DeviceN.mlest.weib,
#                           time.increment = 1,
#                           number.time.units.ahead = 50,
#                           warranty.time = 1000)
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#                             plot.what = "density")
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#                             plot.what = "cumulative")

## ----eval=FALSE----------------------------------------------------------
#  DeviceN.Enforced.weib <-
#    CumulativePredictTable(DeviceN.mlest.weib,
#                           time.increment = 1,
#                           number.time.units.ahead = 50,
#                           warranty.time = 36)
#  
#  PlotCumulativePredictTable(DeviceN.Enforced.weib,
#                             plot.what = "density")
#  
#  PlotCumulativePredictTable(DeviceN.Enforced.weib,
#                             plot.what = "cumulative")

## ----eval=FALSE----------------------------------------------------------
#  PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#                             plot.what = "density",
#                             ylim = c(0,NA))
#  
#  PlotCumulativePredictTable(DeviceN.Enforced.weib,
#                             plot.what = "density",
#                             add = TRUE,
#                             lty = 3,
#                             lwd = 3)
#  
#  PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#                             plot.what = "cumulative")
#  
#  PlotCumulativePredictTable(DeviceN.Enforced.weib,
#                             plot.what = "cumulative",
#                             add = TRUE,
#                             lty = 3,
#                             lwd = 3)

