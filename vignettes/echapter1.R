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

