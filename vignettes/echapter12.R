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

