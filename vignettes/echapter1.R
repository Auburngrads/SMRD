## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
DT::datatable(lzbearing, options = list(pageLength = 8))

## ------------------------------------------------------------------------
lzbearing.ld <- frame.to.ld(lzbearing, 
                            response.column = 1,
                            time.units = "Megacycles")

## ------------------------------------------------------------------------
event.plot(lzbearing.ld)

summary(lzbearing.ld)

print(lzbearing.ld)

plot(lzbearing.ld, distribution = "weibull")

## ------------------------------------------------------------------------
superalloy.ld <- frame.to.ld(superalloy,
                             response.column = 1, 
                             censor.column = 2,
                             x.columns = c(5,6,4),
                             time.units = "Kilocycles")

summary(superalloy.ld)

censored.data.plot(superalloy.ld, 
                   explan.var = 1)

censored.data.plot(superalloy.ld, 
                   explan.var = 3,
                   response.on.yaxis = F)

censored.data.plot(superalloy.ld, 
                   explan.var = 3, 
                   x.axis = "log", 
                   y.axis = "log")

censored.data.plot(superalloy.ld, 
                   explan.var = 3,
                   response.on.yaxis = F, 
                   x.axis = "log", 
                   y.axis = "log")

