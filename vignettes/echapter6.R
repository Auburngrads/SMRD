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

## ------------------------------------------------------------------------
Bleed.ld <- frame.to.ld(bleed,
                        response.column = 1, 
                        censor.column = 2, 
                        case.weight.column = 3,
                        x.columns = 4,
                        time.units = "Hours")

Bleed.ld_D <- ld.split(Bleed.ld, stress.var = "D")
Bleed.ld_Other <- ld.split(Bleed.ld, stress.var = "Other")

## ---- fig.height=9-------------------------------------------------------
event.plot(Bleed.ld)
summary(Bleed.ld)

## ---- fig.height=6-------------------------------------------------------
event.plot(Bleed.ld_D)
summary(Bleed.ld_D)

event.plot(Bleed.ld_Other)
summary(Bleed.ld_Other)

## ------------------------------------------------------------------------
plot(Bleed.ld,my.title="All Bases")

plot(Bleed.ld,
     distribution = "Weibull",
     my.title = "Bleed System Failures\nAll Bases")

plot(Bleed.ld_D,
     distribution = "Weibull",
     my.title = "Bleed System Failures\nOnly Base D")

plot(Bleed.ld_Other,
     distribution = "Weibull",
     my.title = "Bleed System Failures\nOmitting Base D")

## ------------------------------------------------------------------------
probpaper("Weibull",
          xlim = c(1, 10), 
          grid = TRUE, 
          ylim = c(0.011,0.981))

probpaper("Weibull",
          xlim = c(1, 100),
          grid = TRUE,
          ylim = c(0.011,0.981))

probpaper("Weibull",
          xlim = c(1, 1000),
          grid = TRUE,
          ylim = c(0.011,0.981))

probpaper("Weibull",
          xlim = c(1, 1000),
          grid = TRUE, 
          ylim = c(0.0011,0.9981))

probpaper("Lognormal",
          xlim = c(1, 10),
          grid = TRUE,
          ylim = c(0.011,0.981))

probpaper("Lognormal",
          xlim = c(1, 100),
          grid = TRUE,
          ylim = c(0.011,0.981))

probpaper("Lognormal",
          xlim = c(1, 1000),
          grid = TRUE,
          ylim = c(0.011,0.981))

## ---- eval=FALSE---------------------------------------------------------
#  lzbearing.ld <- frame.to.ld(lzbearing, response.column = 1)
#  plot(lzbearing.ld,
#       distribution = "gng",
#       shape = .1,
#       my.title = "gamma = .1",
#       linear.axes = "q")
#  
#  plot(lzbearing.ld,
#       distribution = "gng",
#       shape = .1)
#  
#  multiple.probplot.sim()
#  
#  multiple.probplot.sim(dist="exponential")

