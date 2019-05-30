## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1,
                                censor.column = 3,
                                time.units = "Kilometers")

plot(ShockAbsorber.ld,
     distribution = "Weibull")

plot(ShockAbsorber.ld,
     distribution = "Lognormal")

## ---- fig.width=7, fig.height=5------------------------------------------
plot(ShockAbsorber.ld, 
     distribution = c('weibull', 'sev', 'lognormal', 'normal'))

## ---- fig.width=8--------------------------------------------------------
plot(ShockAbsorber.ld, 
     distribution = c('weibull', 'sev'))

## ------------------------------------------------------------------------
at7987.ld <- frame.to.ld(at7987,
                         response.column = 1, 
                         censor.column = 2, 
                         case.weight.column = 3,
                         time.units = "Thousand Cycles")

plot(at7987.ld,
     distribution = "Weibull")

plot(at7987.ld,
     distribution = "Lognormal")

plot(at7987.ld, 
     distribution = "Weibull",
     plot.censored.ticks = "top")

plot(at7987.ld,
     distribution="Exponential",
     draw.line = .031,
     grid = T, 
     linear.axes = T)

plot(at7987.ld,
     distribution = "Lognormal")

plot(at7987.ld,
     distribution = "Lognormal",
     draw.line = .95,
     grid = T,
     linear.axes = T)

## ------------------------------------------------------------------------
titanium.ld <- frame.to.ld(titanium2,
                           response.column = 1, 
                           censor.column = 2,
                           case.weight.column = 3,
                           time.units = "Hours")

plot(titanium.ld,
     distribution = "Lognormal")

