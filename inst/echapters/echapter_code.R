## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ---- fig.width=7, fig.height=5------------------------------------------
distribution.plot("Weibull",
                  shape = c(1.7),
                  prob.range = c(.000001,.99))

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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

turbine.ld <- frame.to.ld(turbine,
                          response.column = 1,
                          censor.column = 2,
                          case.weight.column = 3,
                          time.units = "Hundreds of Hours")

summary(turbine.ld)

plot(turbine.ld,
     ylim = c(0,1),
     band.type = 'Pointwise')

event.plot(turbine.ld)

plot(turbine.ld,
     band.type = "Simultaneous")

## ------------------------------------------------------------------------
v7tube.ld <- frame.to.ld(v7tube,
                         response.column = c(1,2),
                         censor.column = 3, 
                         case.weight.column = 4,
                         time.units = "Days")

event.plot(v7tube.ld)
summary(v7tube.ld)

plot(v7tube.ld)

plot(v7tube.ld,
     band.type = "Simultaneous")

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------

distribution.plot("Exponential",
                  shape = c(.5,1),
                  xlim = c(0,3))

distribution.plot("Lognormal",
                  shape = c(.3, .8),
                  xlim = c(0,3))

distribution.plot("Normal",
                  shape = c( .30, .5,.8),
                  loc = 5)

distribution.plot("Weibull",
                  shape = c(.8,1,1.5),
                  xlim = c(0,2))

distribution.plot("Smallest Extreme Value",
                  shape = c(5,6,7),
                  loc = 50,
                  xlim = c(30,60))

distribution.plot("Largest Extreme Value",
                  shape = c(5,6,7),
                  loc = 10)

distribution.plot("Logistic",
                  shape = c(1,2,3),
                  loc = 15,
                  xlim = c(5,25))

distribution.plot("Loglogistic",
                  shape = c(.2,.4,.6), 
                  prob.range = c(0.001, 0.95),
                  xlim = c(0,4))

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
distribution.plot("Gamma",
                  shape = c( .8,1,2),
                  xlim = c(0,4))

distribution.plot("Igau",
                  shape = c( 1,2,4),
                  plot.haz.log = F,
                  xlim = c(0,3),
                  prob.range = c(0.001, 0.99))

distribution.plot("Bisa",
                  shape = c(.5,.6,.85,1),
                  plot.haz.log = F, 
                  prob.range = c(0.001, 0.95),
                  xlim = c(0,3))

distribution.plot("Goma",
                  shape = c( .2,2,.2,2),
                  shape2 = c( .5,.5,3,3))

## ---- fig.width=8, fig.height=8------------------------------------------
gets.pdf.plot()

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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
par(mfrow = c(1,1))

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
plot(Bleed.ld,
     my.title = "All Bases")

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
lzbearing.ld <- frame.to.ld(lzbearing, response.column = 1)
plot(lzbearing.ld,
     distribution = "gng",
     shape = .1,
     my.title = "gamma = .1",
     linear.axes = "q")

plot(lzbearing.ld,
     distribution = "gng",
     shape = .1)

multiple.probplot.sim()

multiple.probplot.sim(dist = "exponential")

## ---- echo=FALSE---------------------------------------------------------
library(SMRD)


## ------------------------------------------------------------------------
berkson200.ld <- frame.to.ld(berkson200,
                             response.column = c(1,2),
                             censor.column = 3,
                             case.weight.column = 4,
                             time.units = "1/5000 Seconds")

summary(berkson200.ld)

plot(berkson200.ld)
plot(berkson200.ld, dist = "Exponential")

## ------------------------------------------------------------------------
cdfest(berkson200.ld)

## ------------------------------------------------------------------------
berkson200.mle.exp <- mlest(berkson200.ld, 
                            distribution = "Exponential")

berkson200.mle.exp$ll.text
berkson200.mle.exp$ll.value
berkson200.mle.exp$mttf.text
berkson200.mle.exp$mttf.value
berkson200.mle.exp$mle.table
berkson200.mle.exp$vcv.matrix
berkson200.mle.exp$param.corr.matrix
berkson200.mle.exp$failure.probabilities
berkson200.mle.exp$quantiles
berkson200.mle.exp$hazard.table

## ------------------------------------------------------------------------
mleprobplot(berkson200.ld, 
            distribution = "Exponential", 
            param.loc = "bottomright") 

## ------------------------------------------------------------------------
berkson200.mle.exp <- expon.mle(berkson200.ld)

berkson200.mle.gam <- Gamma.mle(berkson200.ld)

## ------------------------------------------------------------------------
simple.contour(berkson200.ld, 
               distribution = 'exponential', 
               xlim = c(400,800))

## ------------------------------------------------------------------------
compare.many.exponential.profiles(theta = 5,
                                  sample.size = 3,
                                  number.simulation = 10)

compare.many.exponential.profiles(theta = 5,
                                  sample.size = 1000,
                                  number.simulation = 10)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1, 
                                censor.column = 3,
                                time.units = "Kilometers")

## ---- fig.height=12, echo=2:5, fig.width=8-------------------------------
par(mfrow = c(2,2))
mleprobplot(ShockAbsorber.ld, distribution="Weibull")
mleprobplot(ShockAbsorber.ld, distribution = "loglogistic")
mleprobplot(ShockAbsorber.ld, distribution = "lognormal")
mleprobplot(ShockAbsorber.ld, distribution = "frechet")
par(mfrow = c(1,1))

## ---- fig.height=12, fig.width=8, eval=FALSE-----------------------------
#  # This will be replaced with plot.mlest
#  four.mleprobplot(ShockAbsorber.ld)

## ------------------------------------------------------------------------
ShockAbsorber.mlest <- mlest(ShockAbsorber.ld, 
                             distribution = "Weibull")
ShockAbsorber.mlest$ll.text
ShockAbsorber.mlest$mttf.text
ShockAbsorber.mlest$mle.table
ShockAbsorber.mlest$vcv.matrix
ShockAbsorber.mlest$param.corr.matrix
ShockAbsorber.mlest$failure.probabilities
ShockAbsorber.mlest$quantiles

## ---- echo=1:9-----------------------------------------------------------
simple.contour(ShockAbsorber.ld,
               distribution = "lognormal",
               show.confidence = T)


simple.contour(ShockAbsorber.ld,
               distribution = "lognormal",
               show.confidence = T,
               threeD = T)

## ------------------------------------------------------------------------
simple.contour(ShockAbsorber.ld,
               "lognormal",
               show.confidence = F)

simple.contour(ShockAbsorber.ld,
               "lognormal",
               threeD = T)
simple.contour(ShockAbsorber.ld,
               "lognormal",
               quantile = 0.1)

simple.contour(ShockAbsorber.ld,
               "lognormal",
               quantile = 0.3)

simple.contour(ShockAbsorber.ld,
               "lognormal",
               quantile = 0.3,
               log.quantile = T)

simple.contour(ShockAbsorber.ld,
               "lognormal",
               quantile = 0.3,
               rel.or.conf = "")

simple.contour(ShockAbsorber.ld,
               "lognormal",
               quantile = 0.3,
               threeD = T)

## ------------------------------------------------------------------------
failure.probabilities(ShockAbsorber.mlest,
                      time.vec = seq(5000, 50000, by = 2500), 
                      digits = 13)

# compare lognormal and Weibull
compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution = "Lognormal",
                    compare.distribution = "Weibull")

## ------------------------------------------------------------------------
mlehazplot(ShockAbsorber.ld, 
           distribution = "Weibull", 
           param.loc = "topleft")

mlehazplot(ShockAbsorber.ld, 
           distribution = "Frechet")

mlehazplot(ShockAbsorber.ld, 
           distribution = "Lognormal")

mlehazplot(ShockAbsorber.ld, 
           distribution = "Lognormal",
           xlim = c(5000,500000), 
           y.axis = "log")

mlehazplot(ShockAbsorber.ld, 
           distribution = "Weibull",
           time.vec = c(10000,20000,30000))

mlehazplot(ShockAbsorber.ld, 
           distribution = "Weibull",
           time.vec = c(10000,20000,30000),
           parameter.fixed = c(F,T),
           theta.start = c(9.,2))

## ------------------------------------------------------------------------
BearingCage.ld <- frame.to.ld(bearingcage,
                              response.column = 1, 
                              censor.column = 2, 
                              case.weight.column = 3,
                              time.units = "Hours")

summary(BearingCage.ld)


mleprobplot(BearingCage.ld, distribution = "Weibull")

mleprobplot(BearingCage.ld, 
            distribution = "Weibull", 
            parameter.fixed=c(F,T),
            theta.start = c(2,.6667),
            sub.title = "sigma = .667  (or beta = 1.5)")

mleprobplot(BearingCage.ld, 
            distribution = "Weibull", 
            parameter.fixed = c(F,T),
            theta.start = c(9.,.5),
            sub.title = "sigma = .5  (or beta = 2)")

mleprobplot(BearingCage.ld, 
            distribution = "Weibull", 
            parameter.fixed = c(F,T),
            theta.start = c(9.,.3333),
            sub.title = "sigma = .333  (or beta = 3)")

tmpgmle.out <- ls2.mle(BearingCage.ld, 
                       distribution ="Weibull", 
                       theta.start = c(9,.4))

simple.contour(BearingCage.ld,
               distribution = "Weibull",
               zoom.level = 4,
               quantile = .1,
               threeD = TRUE)

simple.contour(BearingCage.ld,
               distribution = "Weibull",
               zoom.level = 4,
               profile = "x")

simple.contour(BearingCage.ld,
               distribution = "Weibull",
               zoom.level = 4,
               profile = "y")


simple.contour(BearingCage.ld,
               distribution = "Weibull",
               threeD = T,
               zoom.level = 3,
               size = 75)

simple.contour(ShockAbsorber.ld,"Weibull", zoom.level = 3)
simple.contour(ShockAbsorber.ld,"Weibull", threeD = T)

tmp <- simple.contour(ShockAbsorber.ld,
                      distribution = "Weibull", 
                      zoom.level = 3, 
                      size = 50)

# The following requires interaction

# newspinp(tmp)

## ------------------------------------------------------------------------
# examples of use of compare.mleprobplot

compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution =  "Lognormal",
                    compare.distribution = "Weibull")

compare.mleprobplot(BearingCage.ld,
                    main.distribution =  "Lognormal",
                    compare.distribution = "Weibull",
                    xlim = c(201,9900),
                    ylim = c(.0001,.5),
                    time.range = c(201,9900))

compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution =  "Lognormal",
                    compare.distribution = c("Weibull","Loglogistic"))

compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution =  "Lognormal",
                    xlim = c(100,100000),
                    ylim = c(.005,.9),
                    compare.distribution = c("Weibull","Loglogistic"))

compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution = "Lognormal",
                    xlim = c(100,50001),
                    ylim = c(.005,.9),
                    band.type = "chull",
                    compare.distribution = c("Weibull", 
                                             "Loglogistic", 
                                             "Exponential"))

compare.mleprobplot(ShockAbsorber.ld, 
                    main.distribution = "Lognormal",
                    xlim = c(100,100000),
                    ylim = c(.005,.9),
                    compare.distribution = c("Weibull","Exponential"),
                    band.type = "chull")

## ------------------------------------------------------------------------
bulb.ld <- frame.to.ld(bulb,
                       response.column = 1,
                       data.title = "Bulb Data",
                       time.units = "Hours")

mlest(bulb.ld,"normal")

simple.contour(bulb.ld,"normal", quantile = .5)

## ---- eval=FALSE---------------------------------------------------------
#  # Here we have the eta parameter on the x axis
#  
#  ShockAbsorber.likelihood.grid <- simple.grid(data.ld = ShockAbsorber.ld,
#                                               distribution = "Weibull")
#  
#  plot.simple.contour(contour.indicators = c(.001,.01,.1,.5,.9),
#                      do.persp = F,
#                      rel.or.conf = "Relative Likelihood",
#                      likelihood.grid.out = ShockAbsorber.likelihood.grid)
#  
#  profile.plot(profile.grid(ShockAbsorber.likelihood.grid, which = "x"))
#  
#  profile.plot(profile.grid(ShockAbsorber.likelihood.grid, which = "y"))
#  profile.plot(profile.grid(ShockAbsorber.likelihood.grid, which = "y"),
#               log.axis = TRUE)
#  
#  # Here we have the 0.1 quantile on the x axis
#  
#  ShockAbsorber.likelihood.grid <- simple.grid(data.ld = ShockAbsorber.ld,
#                                               distribution = "Weibull",
#                                               the.quantile = 0.1)
#  
#  plot.simple.contour(contour.indicators = c(50., 95.),
#                      do.persp=T,
#                      rel.or.conf="Joint confidence region",
#                      the.quantile = 0.1,
#                      likelihood.grid.out = ShockAbsorber.likelihood.grid)
#  
#  plot.simple.contour(contour.indicators = c(50., 95.),
#                      do.persp = F,
#                      rel.or.conf = "Joint confidence region",
#                      the.quantile = 0.1,
#                      likelihood.grid.out = ShockAbsorber.likelihood.grid)
#  
#  plot.simple.contour(contour.indicators = c(.001,.01,.1,.5,.9),
#                      do.persp = F,
#                      rel.or.conf = "Relative Likelihood",
#                      the.quantile = 0.1,
#                      likelihood.grid.out = ShockAbsorber.likelihood.grid)
#  
#  profile.plot(profile.grid(ShockAbsorber.likelihood.grid.out, which = "x"))
#  profile.plot(profile.grid(ShockAbsorber.likelihood.grid.out, which = "y"))
#  
#  plot.simple.contour(contour.indicators=c(50., 95.),
#                      do.persp = TRUE,
#                      rel.or.conf = "Joint confidence region",
#                      the.quantile = 0.1,
#                      likelihood.grid.out = ShockAbsorber.likelihood.grid)

## ---- eval=FALSE---------------------------------------------------------
#  ShockAbsorber.weibull.gmle <- ls.mle(ShockAbsorber.ld,
#                                       distribution = "Weibull")
#  
#  # short test
#  two.dim.profile(ShockAbsorber.weibull.gmle,
#                  profile.on.list = NULL,
#                  which = c(1,2),
#                  size = c(2,2),
#                  range.list = list(c(9.0,10.4),c(-1.8,-.1)),
#                  monitor = 1)
#  
#  two.dim.profile(ShockAbsorber.weibull.gmle,
#                  profile.on.list = NULL,
#                  which = c(1,2),
#                  size = c(25,25),
#                  range.list = list(c(9.0,10.4),c(-1.8,-.1)))
#  
#  profile.contour(ShockAbsorber.weibull.gmle.outstruct1x2,
#                  variable.namey = "log(sigma)")
#  
#  conf.contour(ShockAbsorber.weibull.gmle.outstruct1x2,
#               transformationy = "log",
#               variable.namey = "log(sigma)" )
#  
#  #short test
#  one.dim.profile(ShockAbsorber.weibull.gmle,
#                  size = 2,
#                  range.list = list(c(9.0,10.2),c(-1.8,-.4)))
#  
#  one.dim.profile(ShockAbsorber.weibull.gmle,
#                  size = 200,
#                  range.list = list(c(9.8,10.8),c(-1.8,-.4)))
#  
#  profile.plot(ShockAbsorber.weibull.gmle.outstruct1)
#  
#  profile.plot(transtruct(ShockAbsorber.weibull.gmle.outstruct1,exp),
#               variable.name = "eta")
#  
#  profile.plot(ShockAbsorber.weibull.gmle.outstruct2)
#  
#  profile.plot(transtruct(ShockAbsorber.weibull.gmle.outstruct2,exp),
#               variable.name = "sigma")
#  
#  #define the transformation on the fly to get beta=exp(-log(sigma))
#  tstr = transtruct(ShockAbsorber.weibull.gmle.outstruct2,
#                    function(x){exp(-x)})
#  profile.plot(tstr,
#               variable.name="beta")
#  
#  #short test
#  two.dim.profile(ShockAbsorber.weibull.gmle,
#                  profile.on.list = NULL,
#                  which = c(5,3),
#                  special.stuff.profile = list(spec.quantile = 0.1),
#                  profile.setup = quantile.profile.setup,
#                  profile.stable.parameters = quantile.profile.stable.parameters,
#                  size = c(4,4),
#                  range.list = list(c(9.8,10.8),
#                                    c(-1.8,-.4),
#                                    c(.165,.670),
#                                    NULL,
#                                    c(5000,20000)))
#  
#  two.dim.profile(ShockAbsorber.weibull.gmle,
#                  profile.on.list = NULL,
#                  which = c(5,3),
#                  special.stuff.profile = list(spec.quantile = 0.1),
#                  profile.setup = quantile.profile.setup,
#                  profile.stable.parameters = quantile.profile.stable.parameters,
#                  size = c(20,20),
#                  range.list = list(c(9.8,10.8),
#                                    c(-1.8,-.4),
#                                    c(.165,.670),
#                                    NULL,
#                                    c(5000,20000)))
#  
#  profile.contour(ShockAbsorber.weibull.gmle.outstruct5x3,
#                  variable.namex = "t_.01",
#                  variable.namey = "sigma",
#                  levels = c(0.01, 0.1,0.2, 0.4, 0.7, 0.9))
#  
#  conf.contour(ShockAbsorber.weibull.gmle.outstruct5x3,
#               levels = c(50,90,99))
#  
#  one.dim.profile(ShockAbsorber.weibull.gmle.out,
#                  profile.on.list = 5,
#                  special.stuff.profile = list(spec.quantile = 0.1),
#                  size = 200,
#                  profile.setup = quantile.profile.setup,
#                  profile.stable.parameters = quantile.profile.stable.parameters,
#                  range.list = list(c(5000,20000)),addname = "t.1")
#  
#  one.dim.profile(ShockAbsorber.weibull.gmle.out,
#                  profile.on.list = 5,
#                  special.stuff.profile = list(spec.quantile = .632),
#                  size = 200,
#                  profile.setup = quantile.profile.setup,
#                  profile.stable.parameters = quantile.profile.stable.parameters,
#                  range.list = list(c(5000,20000)),addname = "eta")
#  
#  profile.plot(ShockAbsorber.weibull.gmle.outstruct5t.1)
#  
#  one.dim.profile(ShockAbsorber.weibull.gmle.out,
#                  profile.on.list = 6,
#                  special.stuff.profile = list(spec.time=10000),
#                  size = 200,
#                  profile.setup = quantile.profile.setup, profile.stable.parameters = quantile.profile.stable.parameters,
#                  range.list = list(c(.001,.2)),addname = "F10k")
#  
#  profile.plot(ShockAbsorber.weibull.gmle.outstruct6F10k)
#  
#  one.dim.profile(ShockAbsorber.weibull.gmle.out,
#                  profile.on.list = 6,
#                  special.stuff.profile = list(spec.time=20000),
#                  size = 200,
#                  profile.setup = quantile.profile.setup, profile.stable.parameters = quantile.profile.stable.parameters,
#                  range.list = list(c(.12,.5)),addname = "F20k")
#  
#  profile.plot(ShockAbsorber.weibull.gmle.outstruct6F20k)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1,
                                censor.column = 3,
                                time.units = "Kilometers")

ShockAbsorber.boot.p <- parametric.bootstrap(ShockAbsorber.ld,
                                             distribution = "Weibull",
                                             number.sim = 20)

plot(ShockAbsorber.boot.p)

plot(ShockAbsorber.boot.p, 
     simulate.parameters = TRUE, 
     parameter.sims = 500)

summary(ShockAbsorber.boot.p,
        inference.on = "parameter", 
        which = 1)

summary(ShockAbsorber.boot.p,
        inference.on = "parameter", 
        which = 2,
        do.compare = T)

summary(ShockAbsorber.boot.p,
        inference.on = "parameter", 
        which = 2)

summary(ShockAbsorber.boot.p,
        inference.on = "quantile",
        which = 0.1)

summary(ShockAbsorber.boot.p,
        inference.on = "probability", 
        which = 1000)


summary(ShockAbsorber.boot.p,
        inference.on = "parameter", 
        which = 2,
        do.compare = T)

summary(ShockAbsorber.boot.p,
        inference.on = "parameter", 
        which = 2,
        do.compare = F)

## ------------------------------------------------------------------------
ShockAbsorber.boot.p2 <- parametric.bootstrap(ShockAbsorber.ld,
                                              number.sim = 20,
                                              distribution = "Weibull")

plot(ShockAbsorber.boot.p2)

plot(ShockAbsorber.boot.p2, 
     simulate.parameters = TRUE, 
     parameter.sims = 500)

summary(ShockAbsorber.boot.p2,
        inference.on = "parameter", 
        which = 1)

summary(ShockAbsorber.boot.p2,
        inference.on = "parameter", 
        which = 2)

summary(ShockAbsorber.boot.p2,
        inference.on = "quantile", 
        which = 0.1)

summary(ShockAbsorber.boot.p2,
        inference.on = "probability", 
        which = 1000)

summary(ShockAbsorber.boot.p2,
        inference.on = "parameter", 
        which = 2,
        do.compare = T)

## ------------------------------------------------------------------------
ShockAbsorber.boot.np<- nonparametric.bootstrap(ShockAbsorber.ld,
                                                number.sim = 20)

plot(ShockAbsorber.boot.np)

summary(ShockAbsorber.boot.np, 
        compare = T)

SMRD:::compare.summary.boot.npar.npar.out(ShockAbsorber.boot.np)

## ------------------------------------------------------------------------
ShockAbsorber.boot.np2 <- nonparametric.bootstrap(ShockAbsorber.ld,
                                                  number.sim = 20)

plot(ShockAbsorber.boot.np2)

summary(ShockAbsorber.boot.np2)

SMRD:::compare.summary.boot.npar.npar.out(ShockAbsorber.boot.np2)

## ------------------------------------------------------------------------
BearingCage.ld <- frame.to.ld(bearingcage,
                              response.column = 1, 
                              censor.column = 2, 
                              case.weight.column = 3,
                              time.units = "Hours")

summary(BearingCage.ld)

BearingCage.boot.p <- parametric.bootstrap(BearingCage.ld,
                                           distribution = "Weibull",
                                           number.sim = 20)

plot(BearingCage.boot.p)

summary(BearingCage.boot.p,
        inference.on = "parameter", 
        which = 1)
summary(BearingCage.boot.p,
        inference.on = "parameter", 
        which = 2)
summary(BearingCage.boot.p,
        inference.on = "quantile", 
        which = 0.1)
summary(BearingCage.boot.p,
        inference.on = "probability", 
        which = 1000)

## ------------------------------------------------------------------------
BearingCage.boot.np <- nonparametric.bootstrap(BearingCage.ld,
                                               number.sim = 20)

plot(BearingCage.boot.np)

summary(BearingCage.boot.np)

SMRD:::compare.summary.boot.npar.npar.out(BearingCage.boot.np)

## ------------------------------------------------------------------------
bulb.ld <- frame.to.ld(bulb,
                       response.column = 1,
                       data.title = "Bulb Data",
                       time.units = "Hours")

summary(bulb.ld)

bulb.boot.p <- parametric.bootstrap(bulb.ld,
                                    distribution = "normal",
                                    number.sim = 200)

plot(bulb.boot.p)

summary(bulb.boot.p,
        inference.on = "parameter", 
        which = 1)
summary(bulb.boot.p,
        inference.on = "parameter", 
        which = 2)
summary(bulb.boot.p,
        inference.on = "quantile", 
        which = 0.1)
summary(bulb.boot.p,
        inference.on = "probability", 
        which = 1000)

## ------------------------------------------------------------------------
bulb.boot.np <- nonparametric.bootstrap(bulb.ld,
                                        number.sim = 200)

plot(bulb.boot.np)
summary(bulb.boot.np,
        time.index = 200)

SMRD:::compare.summary.boot.npar.npar.out(bulb.boot.np,
                                   time.index = 200)

## ------------------------------------------------------------------------
SingleDistSim(number.sim = 10,
              distribution = "Weibull",
              theta = c(mu = 0.0, sigma = 1.0),
              sample.size = 10,
              censor.type = "None")

SingleDistSim(number.sim = 10,
              distribution = "Weibull",
              theta = c(mu = 0.0, sigma = 1.0),
              sample.size = 10,
              censor.type = "Type 1", 
              fail.fraction = 0.5)

SingleDistSim(number.sim = 10,
              distribution = "Weibull",
              theta = c(mu = 0.0, sigma = 1.0),
              sample.size = 10,
              censor.type = "Type 2",
              fail.number = 5)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
plan.values1 <- get.plan.values("Weibull", 
                                beta = 2, 
                                prob = .1, 
                                time = 100, 
                                time.units = "Hours")

## ------------------------------------------------------------------------
summary(plan.values1)

plot(plan.values1)
failure.probabilities(plan.values1)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 120,
#                       number.detail = 5,
#                       quantile.mark = 0.2,
#                       number.sim = 200)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 300,
#                       number.detail = 5,
#                       number.sim = 200)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 1000,
#                       number.sim = 50,
#                       quantile.mark = 0.2)

## ------------------------------------------------------------------------
plan.values2 <- get.plan.values("Lognormal", 
                                sigma = 0.5,
                                prob = 0.1, 
                                time = 100, 
                                time.units = "Hours")

summary(plan.values2)
plot(plan.values2)

plot(plan.values2, 
     censor.time = 1000, 
     grids = F)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values2,
#                       n = 50,
#                       censor.time = 1000,
#                       quantile.mark = 0.1)

## ------------------------------------------------------------------------
plan.values3 <- get.plan.values("Weibull",
                                prob = c(.2,.12),
                                time = c(1000,500), 
                                time.units = "Hours")

plan.values4 <- get.plan.values("Weibull",
                                prob = c(.05,.15),
                                time = c(40000,100000),
                                time.units = "Hours")

summary(plan.values3)
plot(plan.values3)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values3,
#                       n = 50,
#                       censor.time = 1000,
#                       quantile.mark = 0.1)

## ------------------------------------------------------------------------
#compare the simulated value with the large-sample approx below

asd.quant(plan.values3, 
          n = 50, 
          censor.time = 1000, 
          quantile.mark = 0.1)

#compare:

asd.quant(plan.values3,
          n = 50, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(50)

asd.quant(plan.values3,
          n = 500, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(500)

asd.quant(plan.values3,
          n = 5000, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(5000)

## ------------------------------------------------------------------------
# For the normal distribution
variance.factor(distribution = 'normal', 
                type = 'quantile', 
                quantile.of.interest = 0.02,
                proportion.failing = 0.2)


# For the smallest extreme value distribution
variance.factor(distribution = 'sev',
                type = 'quantile', 
                quantile.of.interest = 0.02,
                proportion.failing = 0.2)

## ------------------------------------------------------------------------
asym.test.plan.properties(plan.values3, 
                          n = 50, 
                          proportion.failing = 0.1)

asd.quant(plan.values3,
          n = 50, 
          censor.time = 1000, 
          quantile.mark = c(0.1, 0.3, 0.5, 0.63))

## ------------------------------------------------------------------------
lsinf(seq(-1,1, by = 0.1),"right","sev")

lsinf(seq(-2,2, by = 0.2),"right","normal")

## ------------------------------------------------------------------------
table.lines(seq(-1,1,by=.1),"sev")

table.lines(seq(-1,1,by=.1),"normal")

## ------------------------------------------------------------------------
variance.factor("sev", type = 'quantile')
variance.factor("normal", type = 'quantile')
variance.factor("logistic", type = 'quantile')


variance.factor("sev", type = 'hazard')
variance.factor("normal", type = 'hazard')
variance.factor("logistic", type = 'hazard')

## ------------------------------------------------------------------------
zero.failure.plan(xlim = c(1.51,3.99), 
                  ylim = c(.1,29), 
                  krange = c(1.5,3.83))

zero.failure.plan(betavec = c( 1., 2.), 
                  quantile = 0.01, 
                  conlev = 0.95, 
                  xlim = c(1.51,10), 
                  ylim = c(.1,199), 
                  krange = c(1.5,10),
                  grid = T,
                  bw = FALSE)

## ------------------------------------------------------------------------
zero.failure.k(beta = 2, quantile = 0.1, conlev = 0.99,	n = 5)

zero.failure.k(beta = 1, quantile = 0.01, conlev = 0.95, n = 5)

zero.failure.k(beta = 2, quantile = 0.01, conlev = 0.95, n = 5)

## ------------------------------------------------------------------------
zero.failure.n(conlev = 0.95, quantile = 0.01, k = 14, beta = 1)

zero.failure.n(conlev = 0.95, quantile = 0.01, k = 3.369, beta = 2)

zero.failure.prsd(alpha.vec = c(0.05,0.1), quantile = 0.01, pfactor = 3)

## ------------------------------------------------------------------------
bulb.plan.values1 <- get.plan.values("normal", 
                                     sigma = 85, 
                                     prob = 0.5,
                                     time = 1000,
                                     time.units = "Hours")

summary(bulb.plan.values1)
plot(bulb.plan.values1)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(bulb.plan.values1,
#                       n = 50,
#                       censor.time = 1000,
#                       number.detail = 5,
#                       quantile.mark = 0.5)

## ------------------------------------------------------------------------
plot(plan.values3,
     censor.time = 100,
     quantile.of.interest = 0.1)

#here is an example using type 2 censoring

plot(plan.values3,
     fraction.failing = 0.1,
     quantile.of.interest = 0.1)

# In actual application, use number.sim = 10000 to get smoother curves

asym.sample.size(plan.values3,
                 censor.time = 500,
                 Rvalue = 1.5,
                 quantile.of.interest = 0.1)

asym.sample.size(plan.values3,
                 fraction.failing = 0.1,
                 Rvalue = 1.5,
                 quantile.of.interest = 0.1)

asym.sample.size(bulb.plan.values1,
                 fraction.failing = 0.1,
                 HalfWidth = 50,
                 quantile.of.interest = 0.1)

## ------------------------------------------------------------------------
SMRD:::plot.prob.cs.type2("lognormal", 
                            k = 2,
                            n = c(5,10,20),
                            r = c(3,6,12), 
                            number.sim = 100)

SMRD:::plot.prob.cs.type2("loglogistic", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12), 
                           number.sim = 100)

SMRD:::plot.prob.cs.type2("weibull", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12), 
                           number.sim = 100)

SMRD:::plot.prob.cs.type2("frechet", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12),
                           number.sim = 100)

## ---- echo=FALSE---------------------------------------------------------
SMRD:::vinny(cache = T)
library(SMRD)

## ---- eval=FALSE---------------------------------------------------------
#  Fan.egeng.gmle.out <- FillRegion(Fan.egeng.gmle.out,
#                                   nbound = 10,
#                                   iter = 500)
#  
#  summary(Fan.egeng.gmle.out.jcr)
#  
#  names(Fan.egeng.gmle.out.jcr)
#  
#  basic.gmleprobplot(Fan.ld,distribution = "egeng",
#                     xlim = c(200,99999),
#                     ylim = c(.0011,.69),
#                     xxx.mle.out = Fan.egeng.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = .95,
#                     ciMethod = "lr.approx",
#                     length.time.vec = 2)
#  
#  bear.egeng.gmle.out <- egeng.mle(lzbearing.ld)
#  
#  
#  Fan.weibull.gmle.out <- ls2.mle(Fan.ld, distribution = "weibull")
#  
#  
#  Fan.weibull.gmle.out <-  FillRegion(Fan.weibull.gmle.out,
#                                      nbound = 4,
#                                      iter = 50,
#                                      cull = 2 )
#  
#  summary(Fan.weibull.gmle.out.jcr)
#  summary(Fan.weibull.gmle.out)
#  
#  mleprobplot(Fan.ld,
#              distribution = "Weibull",
#              xlim = c(200,9999),
#              ylim = c(.0031,.49))
#  
#  basic.gmleprobplot(Fan.ld,
#                     distribution = "Weibull",
#                     xlim = c(200,9999),
#                     ylim = c(.0031,.49),
#                     xxx.mle.out = Fan.weibull.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = .95,
#                     length.time.vec = 30)
#  
#  basic.gmleprobplot(Fan.ld,
#                     distribution = "Weibull",
#                     xlim = c(200,9999),
#                     ylim = c(.0031,.49),
#                     xxx.mle.out = Fan.weibull.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = 0.95,
#                     ciMethod = "lr.approx",
#                     length.time.vec = 30)

## ---- eval=FALSE---------------------------------------------------------
#  fcn = function(theta,time,distribution){
#  
#        f.phibf((log(time)-theta[1]) / exp(theta[2]),
#                        distribution = "Weibull")
#  
#  }
#  
#  Fr.conf(fcn,
#          fcn.arg2 = log.seq(200,2000,length=10),
#          gmle.out = Fan.weibull.gmle.out,
#          ptwise = T)
#  
#  Fr.conf(fcn,
#          fcn.arg2 = log.seq(200,2000,length = 10),
#          gmle.out = Fan.weibull.gmle.out,
#          ptwise = T,
#          extrapolate = T)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
DT::datatable(gaaslaser, 
              options = list(pageLength=17),
              rownames = FALSE)

## ------------------------------------------------------------------------
GaAsLaser.rmd <- 
  frame.to.rmd(gaaslaser, 
               response.column = 1, 
               unit.column = 2, 
               time.column = 3, 
               response.units = "Increase in Operating Current (%)")

summary(GaAsLaser.rmd)
plot(GaAsLaser.rmd)

## ------------------------------------------------------------------------
trellis.plot(GaAsLaser.rmd, order.groups = T)

trellis.plot(GaAsLaser.rmd, order.groups = F)

## ------------------------------------------------------------------------
GaAsLaser.ld <- SMRD:::rmd.to.ld(GaAsLaser.rmd, 
                          fail.level = 10, 
                          x.axis = "sqrt")

SMRD:::plot.rmd.residual(GaAsLaser.ld)

GaAsLaser.ld <- SMRD:::rmd.to.ld(GaAsLaser.rmd,
                          fail.level = 10)

SMRD:::plot.rmd.residual(GaAsLaser.ld)

summary(GaAsLaser.ld)

## ------------------------------------------------------------------------
mleprobplot(GaAsLaser.ld, dist = "Weibull")
mleprobplot(GaAsLaser.ld, dist = "Lognormal")
mleprobplot(GaAsLaser.ld, dist = "normal")

## ---- error=TRUE---------------------------------------------------------
GaAsLaser.censor.ld <- rmd.to.ld(GaAsLaser.rmd,
                                 fail.level = 8,
                                 censor.time = 3000)

summary(GaAsLaser.censor.ld)

mleprobplot(GaAsLaser.censor.ld, dist = "Weibull")
mleprobplot(GaAsLaser.censor.ld, dist = "Lognormal")
mleprobplot(GaAsLaser.censor.ld, dist = "normal")

## ---- echo = FALSE-------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
BearingCage.ld <- frame.to.ld(bearingcage,
                              response.column = 1, 
                              censor.column = 2,
                              case.weight.column = 3)
DT::datatable(BearingCage.ld)

## ------------------------------------------------------------------------
bcage.prior.weibull.spec1 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 100,
                       qupper = 5000,
                       sigma.dist =  "lognormal",
                       sigma.lower = 0.2,
                       sigma.upper =  0.5,
                       distribution = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.weibull.spec2 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.5,
                       sigma.upper = 2.5, 
                       distribution  = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.weibull.spec3 <-
  specify.simple.prior(p = .01,
                       qdist = "lognormal",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.5,
                       sigma.upper = 2.5,
                       distribution  = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec1 <-
  specify.simple.prior( p  = .04,
                        qdist = "loguniform",
                        qlower = 100,
                        qupper = 5000,
                        sigma.dist = "lognormal",
                        sigma.lower = 0.2,
                        sigma.upper = 5,
                        distribution  = "Lognormal")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec2 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1,
                       sigma.upper =  1.5,
                       distribution = "Lognormal")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec3 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.,
                       sigma.upper = 1.5,
                       distribution  = "Lognormal")

## ------------------------------------------------------------------------
prior2.bcage <- 
  make.prior(spec = bcage.prior.lognormal.spec1, 
             number.in.prior = 3000)


prior.and.post2.bcage <-
  get.big.posterior(bcage.prior.lognormal.spec1,
                    BearingCage.ld)

prior.and.post2.bcage$post[1:10,]

prior.and.post3.bcage <- 
  make.small.posterior.object(prior.and.post2.bcage)

## ------------------------------------------------------------------------
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = T,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

#quantle marginal
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#sigma marginal
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = T,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#prob
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = T)

#Joint only axes.range.default.post = T 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             axes.range.default.post = T,
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = T)

#Joint only 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

#Joint only 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#Joint only axes.range.default.post = F 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             axes.range.default.post = F,
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = F)

#Joint only axes.range.default.post = F 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             axes.range.default.post = F, 
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = F)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu")

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 6000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Parameter",
                             newdata = "mu")

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1)

summarize.posterior.or.prior(prior.and.post2.bcage,post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 6000)


prior.and.post3.bcage <- make.small.posterior.object(prior.and.post2.bcage)

SMRD:::plot.prediction(prior.and.post2.bcage, 
                time.range = log(c(500,20000000)),
                xlab = "Hours")

## ------------------------------------------------------------------------
SMRD:::plot.prediction.order(x = 1,
                      nsamsize = 3,
                      prior.and.post2.bcage,
                      time.range = log(c(50,200000)),
                      xlab = "Hours")

## ------------------------------------------------------------------------
SMRD:::plot.prediction.order(x = 1,
                      nsamsize = 50,
                      prior.and.post2.bcage,
                      time.range = log(c(10,100000)),
                      xlab = "Hours")

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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

## ------------------------------------------------------------------------
par(mfrow = c(1,2))
mfmi.mleprobplot(DeviceG.ld, distribution = "weibull") 
mfmc.mleprobplot(DeviceG.ld, distribution = "lognormal") 
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
mfmc.mleprobplot(DeviceG.ld, 
                 distribution = "Weibull") 

mfmc.mleprobplot(DeviceG.ld, 
                 distribution = "Weibull", 
                 band.type = "none")

tmp <- mfmc.mleprobplot(DeviceG.ld, 
                       distribution = "Weibull", 
                       band.type = "none", 
                       show.individual = F, 
                       ylim = c(0.1, .99))

failure.probabilities(tmp)
quantiles(tmp)

## ------------------------------------------------------------------------
tmpx <- mfmc.mleprobplot(DeviceG.ld, 
                         distribution = "Weibull", 
                         distribution.vec = c("Weibull","Lognormal"))

failure.probabilities(tmpx)
quantiles(tmpx)

## ------------------------------------------------------------------------
ShockAbsorber.ld <- frame.to.ld(shockabsorber,
                                response.column = 1,
                                failure.mode.column = 2,
                                censor.column = 3, 
                                time.units = "Kilometers")
summary(ShockAbsorber.ld)
event.plot(ShockAbsorber.ld)

## ------------------------------------------------------------------------
mleprobplot(ShockAbsorber.ld, 
            distribution = "Weibull")

mfmi.mleprobplot(ShockAbsorber.ld, 
                 distribution = "Weibull")

tmpx <- mfmc.mleprobplot(ShockAbsorber.ld, 
                         distribution = "Weibull")

failure.probabilities(tmpx)
quantiles(tmpx)

## ------------------------------------------------------------------------
ShockAbsorber.mfld <- mfm.to.ld(ShockAbsorber.ld)

multiple.mleprobplot(ShockAbsorber.mfld,
                     data.ld.name="xx",
                     xlab="yy",
                     distribution="Weibull")

mleprobplot(ShockAbsorber.Mode1.ld, 
            distribution = "Weibull")

mleprobplot(ShockAbsorber.Mode2.ld,
            distribution = "Weibull")

get.time.vector(ShockAbsorber.Mode2.ld)

## ------------------------------------------------------------------------
ConnectionStrength.ld <- 
  frame.to.ld(connectionstrength,
              response.column = 1,
              failure.mode.column = 2,
              case.weight.column = 3)

summary(ConnectionStrength.ld )
event.plot(ConnectionStrength.ld)

mfm.to.ld(ConnectionStrength.ld)

mleprobplot(ConnectionStrength.Bond.ld , 
            distribution = "normal")

mlest(ConnectionStrength.Bond.ld , 
      distribution = "normal")

## ---- eval=FALSE---------------------------------------------------------
#  gmlest(ConnectionStrength.Bond.ld ,
#         distribution = "normal")

## ------------------------------------------------------------------------
mfmi.mleprobplot(ConnectionStrength.ld,
                 distribution = "Normal")

tpmx <- mfmc.mleprobplot(ConnectionStrength.ld,
                         distribution = "Normal")

failure.probabilities(tmpx)
quantiles(tmpx)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ----workstation---------------------------------------------------------
#time.column, ID.column, cost.count.column, event.column
WorkStation.rdu <- frame.to.rdu(workstation,
                                ID.column = "station",
                                time.column = "days",
                                event.column = "event")

#attr(WorkStation.rdu,"WindowInfo")

WorkStation.mcf <- mcf(WorkStation.rdu)

#sqrt(WorkStation.mcf$Var)

event.plot(WorkStation.rdu)

plot(WorkStation.mcf)

## ----computerlab---------------------------------------------------------
ComputerLab.rdu <- frame.to.rdu(computerlab,
                                ID.column = "computer",
                                time.column = "days",
                                event.column = "event")

#attr(ComputerLab.rdu,"WindowInfo")

ComputerLab.mcf <- mcf(ComputerLab.rdu)

#sqrt(ComputerLab.mcf$Var)

event.plot(ComputerLab.rdu)

plot(ComputerLab.mcf)

## ----valveseat-----------------------------------------------------------
ValveSeat.rdu <- frame.to.rdu(valveseat,
                              ID.column = "engine", 
                              time.column = "days" ,
                              event.column = "event", 
                              data.title = "Valve-Seat Replacement Data",
                              time.units = "Days")

#attr(ValveSeat.rdu,"WindowInfo")

summary(ValveSeat.rdu)

event.plot(ValveSeat.rdu)

mcf.plot(ValveSeat.rdu)

## ----cylinder------------------------------------------------------------
Cylinder.rdu <- frame.to.rdu(cylinder,
                             ID.column = "engine", 
                             time.column = "days", 
                             event.column = "event", 
                             cost.count.column = "count", 
                             data.title = "Cylinder Replacement Data",
                             time.units = "Days")

#attr(Cylinder.rdu,"WindowInfo")

PlotMCFandNHPP(Cylinder.rdu, form = "power rule")

Cylinder.mcf <- mcf(Cylinder.rdu)

plot(Cylinder.mcf)

## ----grids1--------------------------------------------------------------
Grids1.rdu <- frame.to.rdu(grids1,
                           ID.column = "unit", 
                           time.column = "days",
                           event.column = "event",
                           data.title = "Grids1 Replacement Data",
                           time.units = "Days")

summary(Grids1.rdu)

event.plot(Grids1.rdu)
print(mcf(Grids1.rdu))
mcf.plot(Grids1.rdu)

#attr(Grids1.rdu,"WindowInfo")

PlotMCFandNHPP(Grids1.rdu, form = "power rule")
Grids1.mcf <- mcf(Grids1.rdu)
plot(Grids1.mcf)

## ----grids2--------------------------------------------------------------
Grids2.rdu <- frame.to.rdu(grids2, 
                           time.column = c(2), 
                           event.column = 3,
                           ID.column = 1, 
                           data.title = "Grids2 Replacement Data",
                           time.units ="Days")

summary(Grids2.rdu)

#attr(Grids2.rdu,"WindowInfo")

PlotMCFandNHPP(Grids2.rdu,
               form = "power rule")
Grids2.mcf <- mcf(Grids2.rdu)
plot(Grids2.mcf)

event.plot(Grids2.rdu)
print(mcf(Grids2.rdu))
mcf.plot(Grids2.rdu)

mcf.diff.plot(Grids1.rdu,
              Grids2.rdu,
              plot.seg = T,
              xlab = "Locomotive Age in Days",
              ylab = "Difference in Mean Cumulative Replacements")

## ----halfbeak------------------------------------------------------------
halfbeak.rdu <- frame.to.rdu(halfbeak,
                             ID.column = "unit", 
                             time.column = "hours" ,
                             event.column = "event", 
                             data.title = "Halfbeak Data", 
                             time.units = "Thousands of Hours of Operation")

summary(halfbeak.rdu)
event.plot(halfbeak.rdu)
print(mcf(halfbeak.rdu))
mcf.plot(halfbeak.rdu)
interarrival.times(halfbeak.rdu)
mcf.plot(halfbeak.rdu,
         xlab = "Thousands of Hours of Operation",
         ylab = "Cumulative Number of Maintenance Actions")

## ----halfbeak2-----------------------------------------------------------
interarrival.plot(halfbeak.rdu,
                  xlab = "Thousands of Hours of Operation",
                  ylab = "Thousands of Hours Between Maintenance Actions",
                  my.title = "")

ar1.plot(halfbeak.rdu,
         xlab = "Lagged Thousands of Hours Between Maintenance Actions",
         ylab = "Thousands of Hours Between Maintenance Actions")

ar1.plot(halfbeak.rdu,
         xlab = "Lagged Thousands of Hours Between Maintenance Actions",
         ylab = "Thousands of Hours Between Maintenance Actions",
         plot.acf = T)

fit.power.and.loglin.process(halfbeak.rdu,
                             xlab = "Thousands of Hours of Operation",
                             ylab = "Cumulative Number of Maintenance Actions")
legend(SMRD:::x.loc(.01),
       SMRD:::y.loc(.95),
       legend = c("Nonparametric MCF estimate",
                  "Log-linear Recurrence Rate NHPP MCF",
                  "Power Recurrence Rate NHPP MCF"),
       lty = c(1,1,3),
       lwd = c(3,1,1))


repair.tsplot(halfbeak.rdu)
interarrival.plot(halfbeak.rdu)
ar1.plot(halfbeak.rdu)

renewal.plots(halfbeak.rdu)
laplace.test(halfbeak.rdu)
lewis.robinson.test(halfbeak.rdu)
milhbk189.test(halfbeak.rdu)

PlotMCFandNHPP(halfbeak.rdu, form = "power rule")
PlotMCFandNHPP(halfbeak.rdu, form = "log linear")

## ------------------------------------------------------------------------
#fit.power.process(halfbeak.rdu)
#fit.loglin.process(halfbeak.rdu)
fit.power.and.loglin.process(halfbeak.rdu)

TestWindow(halfbeak[[1]],halfbeak[[2]],halfbeak[[3]],NULL)
event.plot(halfbeak.rdu )
attr(halfbeak.rdu,"WindowInfo")

TestWindow(halfbeak[[1]],halfbeak[[2]],halfbeak[[3]],NULL)

RiskSet(halfbeak.rdu)

halfbeak.mcf.out <- mcf(halfbeak.rdu)
plot(halfbeak.mcf.out )
fit.power.and.loglin.process(halfbeak.rdu)

NHPP.mle(halfbeak.rdu,
         form = "power rule")
NHPP.mle(halfbeak.rdu,
         form = "log linear")

PlotMCFandNHPP(halfbeak.rdu,
               form = "log linear")
PlotMCFandNHPP(halfbeak.rdu,
               form = "power rule")

## ----grampus-------------------------------------------------------------
grampus.rdu <- frame.to.rdu(grampus, 
                            time.column = c(2), 
                            event.column = 3,
                            data.title = "Grampus Data", 
                            ID.column = 1, 
                            time.units ="Thousands of Hours of Operation")

summary(grampus.rdu)

event.plot(grampus.rdu)
print(mcf(grampus.rdu))

mcf(grampus.rdu)

mcf.plot(grampus.rdu)

interarrival.times(grampus.rdu)
repair.tsplot(grampus.rdu)
interarrival.plot(grampus.rdu)
ar1.plot(grampus.rdu)

renewal.plots(grampus.rdu)
milhbk189.test(grampus.rdu)
lewis.robinson.test(grampus.rdu)
laplace.test(grampus.rdu)

PlotMCFandNHPP(grampus.rdu,form="power rule")
PlotMCFandNHPP(grampus.rdu,form="log linear")

fit.power.and.loglin.process(grampus.rdu)

## ----grampus2------------------------------------------------------------
mleprobplot(interarrival.times(grampus.rdu),"Weibull")

mcf.plot(grampus.rdu,
         xlab = "Thousands of Hours of Operation",
         ylab = "Cumulative Number of Maintenance Actions")

interarrival.plot(grampus.rdu,
                  xlab = "Thousands of Hours of Operation",
                  ylab = "Thousands of Hours Between Maintenance Actions",
                  my.title = "")

ar1.plot(grampus.rdu,
         xlab = "Lagged Thousands of Hours Between Maintenance Actions",
         ylab = "Thousands of Hours Between Maintenance Actions",
         my.title = "")

fit.power.and.loglin.process(grampus.rdu,
                             xlab = "Thousands of Hours of Operation",
                             ylab = "Cumulative Number of Maintenance Actions")
legend(SMRD:::x.loc(.02), 
       SMRD:::y.loc(.98),
       legend = c("Nonparametric MCF estimate",
                  "Log-linear Recurrence Rate NHPP MCF",
                  "Power Recurrence Rate NHPP MCF"),
       lty=c(1,1,3),
       lwd=c(3,1,1))

lewis.robinson.test(grampus.rdu)

## ----machineh------------------------------------------------------------
MachineH.rdu <- frame.to.rdu(machineh,
                             ID.column = "unit", 
                             time.column = "hours",
                             event.column = "event", 
                             cost.count.column = "cost", 
                             data.title = "Earth-Moving Machine Repair Labor Hours", 
                             time.units = "Hours of Operation")

event.plot(MachineH.rdu,
           my.title = "", 
           xlab = "Hours of Operation", 
           which.system.to.plot = 1:23,
           ylab = "Machine Number")


mcf.plot(MachineH.rdu, 
         ylab = "Mean Cumulative Number of Labor Hours",
         plot.seg = T)

event.plot(MachineH.rdu)
attr(MachineH.rdu,"WindowInfo")

MachineH.mcf <- mcf(MachineH.rdu)
plot(MachineH.mcf)
PlotMCFandNHPP(MachineH.rdu, form = "power rule")

## ----r4490---------------------------------------------------------------
R4490.rdu <- frame.to.rdu(r4490,
                          ID.column = "vin",
                          time.column = "days" , 
                          cost.count.column = "costcount" ,
                          event.column = "code")

attr(R4490.rdu,"WindowInfo")

event.plot(R4490.rdu)
R4490.mcf <- mcf(R4490.rdu)
plot(R4490.mcf)

## ---- eval=FALSE---------------------------------------------------------
#  R4490.nhpp.out <- PlotMCFandNHPP(R4490.rdu, form = "power rule")
#  
#  one.dim.profile(R4490.nhpp.out,
#                  size = 5,
#                  save.s = T)
#  
#  two.dim.profile(R4490.nhpp.out,
#                  profile.on.list = NULL,
#                  which = c(1,2),
#                  size = c(5,5))
#  
#  profile.contour(R4490.nhpp.outstruct1x2,
#                  transformationy = "log",
#                  variable.namey = "sigma",
#                  variable.namex = "mu",
#                  v = c(0.001, 0.01, .1,0.2, 0.4, 0.7, 0.9) )

## ----hpcrepairs----------------------------------------------------------
HPCRepairs.rdu <- frame.to.rdu(hpcrepairs,
                               ID.column = "system", 
                               time.column = "months" , 
                               event.column = "event")

attr(HPCRepairs.rdu,"WindowInfo")

PlotMCFandNHPP(HPCRepairs.rdu,
               form = "power rule")

HPCRepairs.mcf <- mcf(HPCRepairs.rdu)
plot(HPCRepairs.mcf)

## ----amsaaexactfail, eval=FALSE------------------------------------------
#  TestWindow(amsaaexactfail[[1]],
#             amsaaexactfail[[2]],
#             amsaaexactfail[[3]],
#             NULL)
#  
#  AMSAAExactFail.rdu <- frame.to.rdu(amsaaexactfail,
#                                     ID.column = "vehicle",
#                                     time.column = "miles" ,
#                                     event.column = "event")
#  
#  names(attributes(AMSAAExactFail.rdu))
#  
#  # testing the loglikelihood
#  theta.mat <- matrix(c(1,1,2,2),2,2)
#  #theta.mat <- c(1,2)
#  loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "power.law")
#  
#  TestLike(AMSAAExactFail.rdu, theta.mat, "power.law")
#  
#  theta.mat <- matrix(c(.01,.01,.02,.02),2,2)
#  loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "log.linear")
#  
#  attr(AMSAAExactFail.rdu,"WindowInfo")
#  
#  RiskSet(AMSAAExactFail.rdu)
#  
#  plotRiskSet(AMSAAExactFail.rdu,proportion = T)
#  
#  get.UnitID(AMSAAExactFail.rdu)
#  
#  mcf(AMSAAExactFail.rdu)
#  
#  event.plot(AMSAAExactFail.rdu)
#  
#  plotRiskSet(AMSAAExactFail.rdu,proportion = T)
#  
#  plot(mcf(AMSAAExactFail.rdu))
#  
#  PlotMCFandNHPP(AMSAAExactFail.rdu,
#                 form = "log linear")
#  
#  AMSAAExactFail.nhpp.out <-
#    PlotMCFandNHPP(AMSAAExactFail.rdu,
#                   form = "power rule")
#  
#  one.dim.profile(AMSAAExactFail.nhpp.out,
#                  size = 5,
#                  save.s = T)

## ----amsaawindow1, eval=FALSE--------------------------------------------
#  AMSAAWindow1.rdu <- frame.to.rdu(amsaawindow1,
#                                   ID.column ="vehicle",
#                                   time.column ="miles",
#                                   event.column = "event")
#  
#  attr(AMSAAWindow1.rdu,"WindowInfo")
#  
#  event.plot(AMSAAWindow1.rdu)
#  
#  RiskSet(AMSAAWindow1.rdu, JustEvent=F)
#  plotRiskSet(AMSAAWindow1.rdu)
#  plotRiskSet(AMSAAWindow1.rdu,proportion=T)
#  
#  plot(mcf(AMSAAWindow1.rdu))
#  
#  PlotMCFandNHPP(AMSAAWindow1.rdu,form = "log linear")
#  
#  AMSAAWindow1.nhpp.out<-
#    PlotMCFandNHPP(AMSAAWindow1.rdu,
#                   form = "power rule")
#  
#  one.dim.profile(AMSAAWindow1.nhpp.out,
#                  size = 5,
#                  save.s = T)

## ----amsaawindow2, eval=FALSE--------------------------------------------
#  AMSAAWindow2.rdu <- frame.to.rdu(amsaawindow2,
#                                   ID.column = "vehicle",
#                                   time.column = "miles" ,
#                                   event.column = "event")
#  
#  attr(AMSAAWindow2.rdu,"WindowInfo")
#  
#  event.plot(AMSAAWindow2.rdu)
#  
#  RiskSet(AMSAAWindow2.rdu)
#  RiskSet(AMSAAWindow2.rdu, JustEvent=F)
#  plotRiskSet(AMSAAWindow2.rdu)
#  plotRiskSet(AMSAAWindow2.rdu,proportion=T)
#  mcf(AMSAAWindow2.rdu)
#  
#  
#  plot(mcf(AMSAAWindow2.rdu))

## ---- eval=FALSE---------------------------------------------------------
#  PlotMCFandNHPP(AMSAAWindow2.rdu,
#                 form = "log linear")
#  
#  AMSAAWindow2.nhpp.out <-  PlotMCFandNHPP(AMSAAWindow2.rdu,
#                                           form = "power rule")
#  
#  one.dim.profile(AMSAAWindow2.nhpp.out,
#                  size = 5,
#                  save.s = T)
#  
#  two.dim.profile(AMSAAWindow2.nhpp.out,
#                  profile.on.list = NULL,
#                  which = c(1,2),
#                  size = c(5,5))
#  
#  profile.contour(AMSAAWindow2.nhpp.outstruct1x2,
#                  transformationy = "log",
#                  variable.namey = "sigma",
#                  variable.namex = "mu",
#                  v = c(0.001, 0.01, 0.1, 0.2, 0.4, 0.7, 0.9))
#  
#  AMSAAWindow2.nhpp.loglin.out <-
#    PlotMCFandNHPP(AMSAAWindow2.rdu,
#                   form = "log linear")
#  
#  two.dim.profile(AMSAAWindow2.nhpp.loglin.out,
#                  which = c(1,2),
#                  size = c(9,9))
#  
#  two.dim.profile(AMSAAWindow2.nhpp.loglin.out,
#                  profile.on.list = NULL,
#                  which = c(1,2),
#                  size = c(5,5),
#                  range.list = list(c(1.6,2.4),c(.00010,.00016)))
#  
#  profile.contour(AMSAAWindow2.nhpp.loglin.outstruct1x2,
#                  transformationy = "log",
#                  variable.namey = "sigma",
#                  variable.namex = "mu",
#                  v = c(0.001, 0.01, .1,0.2, 0.4, 0.7, 0.9) )

## ---- eval=FALSE---------------------------------------------------------
#  test.rdu <- frame.to.rdu(test,
#                           ID.column = "Unit",
#                           time.column = "Hours",
#                           event.column = "Event",
#                           data.title = "Test Data",
#                           time.units = "Thousands of Hours of Operation")
#  
#  summary(test.rdu)
#  
#  event.plot(test.rdu)
#  
#  event.plot(test.rdu)
#  print(mcf(test.rdu))
#  mcf.plot(test.rdu)
#  interarrival.times(test.rdu)
#  
#  mcf.plot(test.rdu,
#           xlab="Thousands of Hours of Operation",
#           ylab="Cumulative Number of Maintenance Actions")
#  
#  interarrival.plot(test.rdu,
#                    xlab = "Thousands of Hours of Operation",
#                    ylab = "Thousands of Hours Between Maintenance Actions",
#                    my.title = "")
#  
#  ar1.plot(test.rdu,
#           xlab = "Lagged Thousands of Hours Between Maintenance Actions",
#           ylab = "Thousands of Hours Between Maintenance Actions")
#  
#  ar1.plot(test.rdu,
#           xlab = "Lagged Thousands of Hours Between Maintenance Actions",
#           ylab = "Thousands of Hours Between Maintenance Actions",
#           plot.acf = T)
#  
#  fit.power.and.loglin.process(test.rdu,
#                               xlab = "Thousands of Hours of Operation",
#                               ylab = "Cumulative Number of Mx Actions")
#  legend(1.55474,
#         63.7603,
#         legend = c("Nonparametric MCF estimate",
#                    "Log-linear Recurrence Rate NHPP MCF",
#                    "Power Recurrence Rate NHPP MCF"),
#         lty = c(1,1,3),
#         lwd = c(3,1,1))
#  
#  
#  repair.tsplot(test.rdu)
#  interarrival.plot(test.rdu)
#  ar1.plot(test.rdu)
#  
#  renewal.plots(test.rdu)
#  laplace.test(test.rdu)
#  lewis.robinson.test(test.rdu)
#  milhbk189.test(test.rdu)
#  dump(c("loglikeNHPP",
#         "TestLike",
#         "Sxloglikenhpp",
#         "flogrecurrate",
#         "fmcfdiff",
#         "flogrecurratepower",
#         "flogrecurrateloglin",
#         "fmcf",
#         "fmcfpower",
#         "fmcfloglin"),"nhpp.q")

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

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
#  SMRD:::resid.vs.order(comptime.mlest.out)
#  
#  SMRD:::resid.vs.fit(comptime.mlest.out)
#  
#  SMRD:::resid.vs.explan(comptime.mlest.out)
#  
#  SMRD:::resid.probplot(comptime.mlest.out)
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

SMRD:::resid.vs.order(ZelenCap.groupm.out1)

SMRD:::resid.vs.fit(ZelenCap.groupm.out1)

SMRD:::resid.vs.explan(ZelenCap.groupm.out1,
                        x.to.plot = 1)

SMRD:::resid.vs.explan(ZelenCap.groupm.out1,
                        x.to.plot = 2)

SMRD:::resid.probplot(ZelenCap.groupm.out1)

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
#  SMRD:::resid.vs.explan.multiple(ZelenCap.groupm.out3)
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

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ---- eval=!FALSE--------------------------------------------------------
advbond.model1 <- 
  get.alt.plan.values.from.two.points(
    distribution = "Weibull",
    relationship = "Inverse Power Rule", 
    time.units = "hours", 
    censor.time = 1000,
    probs = c(.001,.9), 
    accelvar.units = "volts", 
    accelvar = c(110,150), 
    beta = 1.667)

advbond.test.plan1 <- 
  get.alt.test.plan.direct(accel.variable.levels = c(130,140,150),
                           number.of.units = c(100,100,100))

plot(advbond.test.plan1,
     ALT.plan.values = advbond.model1, 
     my.title = "",
     use.conditions = 120)

advbond.model2 <- 
  get.alt.plan.values.from.two.points(distribution = "Lognormal",	
                                      relationship = "Arrhenius", 
                                      time.units = "days",
                                      censor.time = 183,
                                      probs = c(.001,.9),
                                      accelvar = c(50,120),
                                      sigma = .5)


advbond.test.plan2 <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                                               number.of.units = c(100,100,100))

plot(advbond.test.plan2, 
     ALT.plan.values = advbond.model2, 
     my.title = "",
     use.condition = 50)

## ------------------------------------------------------------------------
AF(160,80,.8,"arrhenius")

AFplot(160,80,.8,"arrhenius")

AFplot(160,80,c(.7,.8,.9),"arrhenius")

AF(130,110,-2,"Inverse Power Rule")

AFplot(130,110,-2,"Inverse Power Rule")

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
DeviceA.ld <- frame.to.ld(devicea, 
                          data.title = "Device-A ALT Results",
                          response.column = 1,
                          time.units = "Hours",
                          censor.column = 2,
                          case.weight.column = 3,
                          x.columns = 4, 
                          xlab = "Degrees C")

print(DeviceA.ld)
summary(DeviceA.ld)

censored.data.plot(DeviceA.ld)

censored.data.plot(DeviceA.ld, 
                   y.axis ="log", 
                   x.axis = "Arrhenius")

groupi.mleprobplot(DeviceA.ld, 
                   distribution = "Weibull")

four.groupi.mleprobplot(DeviceA.ld)

DeviceA.weib.groupi <- 
  groupi.mleprobplot(DeviceA.ld,
                     distribution = "Weibull")

print(DeviceA.weib.groupi)
summary(DeviceA.weib.groupi)

DeviceA.lognor.groupi <- 
  groupi.mleprobplot(DeviceA.ld,
                     distribution = "Lognormal")

summary(DeviceA.lognor.groupi)

failure.probabilities(DeviceA.lognor.groupi)
quantiles(DeviceA.lognor.groupi)

four.groupm.mleprobplot(DeviceA.ld, 
                        relationship = "Arrhenius")

DeviceA.lognor.groupm <- 
  groupm.mleprobplot(DeviceA.ld, 
                     distribution = "Lognormal", 
                     relationship = "Arrhenius")

failure.probabilities(DeviceA.lognor.groupm, 
                      new.data = 10)

quantiles(DeviceA.lognor.groupm, 
          new.data = "10")

plot(DeviceA.lognor.groupm)

## or, more specifically to get quantiles at 60 degrees C,

quantiles(DeviceA.lognor.groupm, 
          new.data = 60)

## failure probabilities at 60 and 10 degrees C,

failure.probabilities(DeviceA.lognor.groupm, 
                      new.data = 60, 
                      time = c(1000,2000,5000,10000,20000,50000))

failure.probabilities(DeviceA.lognor.groupm, 
                      new.data = 10, 
                      time = c(1000,2000,5000,10000,20000,50000))

plot(DeviceA.lognor.groupm, 
     censor.time = 5000, 
     quant.lines = c(.01,.05,.1))

quantiles(DeviceA.lognor.groupm, 
          new.data = 60, 
          to = 0.2)

failure.probabilities(DeviceA.lognor.groupm, 
                      new.data = 60, 
                      time.vec = c(10000,100000))

failure.probabilities(DeviceA.lognor.groupm, 
                      new.data = 60, 
                      time.vec = c(500,600,700,800,900,1000))

## plot with a confidence interval for 10 degrees

DeviceA.groupm.mleprobplot <- 
  groupm.mleprobplot(DeviceA.ld,
                     distribution = "Lognormal",
                     relationship = "Arrhenius",
                     ci.list = 1)

print(DeviceA.groupm.mleprobplot,
      print.vcv = T,
      stress.state = 0)

## fix the activation energy to EA=.72

groupm.mleprobplot(DeviceA.ld, 
                   distribution = "Lognormal", 
                   relationship = "Arrhenius", 
                   ci.list = 1,
                   theta.start = c(-13, 0.72, 0.99), 
                   parameter.fixed = c(F, T, F))

## ------------------------------------------------------------------------
mylarpoly.ld <- frame.to.ld(mylarpoly,
                            response.column = 1,
                            x.column = 2, 
                            data.title = "Mylar-Polyurethane Insulating Structure", 
                            time.units = "Minutes", 
                            xlab = "kV/mm")

mylarsub.ld <- frame.to.ld(mylarsub,
                           response.column = 1,
                           x.column = 2,
                           data.title = "Mylar-Polyurethane Insulating Structure", 
                           time.units = "Minutes", 
                           xlab = "kV/mm")

print(mylarpoly.ld)
summary(mylarpoly.ld)

censored.data.plot(mylarpoly.ld, 
                   x.axis = "log", 
                   y.axis = "log")

mylarpoly.lognor.groupi <- 
  groupi.mleprobplot(mylarpoly.ld,
                     distribution = "Lognormal")

text(-1.06459, -1.74056,"361.4 kV/mm")
text(2.23807, -2.1,"219.0")
text(3.88939, -2.1,"157.1")
text(5.23223, -2.1,"122.4")
text(6.90171, -2.1,"100.3")

summary(mylarpoly.lognor.groupi,
        stress.state = 0)

mylarsub.lognor.groupm <- 
  groupm.mleprobplot(mylarsub.ld,
                     distribution = "Lognormal",
                     relationship = c("log"),
                     new.data = c(50))

text(2.50825, -2.27,"219.0")
text(3.83294, -2.27,"157.1")
text(4.84914, -2.27,"122.4")
text(5.95607, -2.27,"100.3")
text(9.56722, -2.26083,"50 kV/mm")

print(mylarsub.lognor.groupm,
      stress.state = 0)

summary(mylarsub.lognor.groupm,
        stress.state = 0)

plot(mylarsub.lognor.groupm)

plot(mylarsub.lognor.groupm, 
     data.ld = mylarpoly.ld, 
     add.density.at = 50,
     xlim = c(31,400))

quantiles(mylarsub.lognor.groupm, 
          new.data = 60, 
          to = 0.2)

failure.probabilities(mylarsub.lognor.groupm,
                      new.data = 60)

failure.probabilities(mylarsub.lognor.groupm, 
                      new.data = 60, 
                      time.vec = c(10,1000))


failure.probabilities(mylarsub.lognor.groupm, 
                      new.data = 50)

quantiles(mylarsub.lognor.groupm, 
          new.data = 50,
          to = 0.2)

mylarpoly.lognor.groupm <- 
  groupm.mleprobplot(mylarpoly.ld,
                     distribution = "Lognormal",
                     relationship = "class")

## example of bad model

mylarpoly.lognor.groupm <- 
  groupm.mleprobplot(mylarpoly.ld,
                     distribution = "Lognormal",
                     relationship = "log")

plot(mylarpoly.lognor.groupm)

mylarsub.lognor.groupm <- 
  groupm.mleprobplot(mylarsub.ld,
                     distribution = "Lognormal",
                     relationship = "log")

summary(mylarsub.lognor.groupm,
        stress.state = 0)

quantiles(mylarsub.lognor.groupm, 
          new.data = 50,
          to = 0.2)

mylarpoly.lognor.groupm <- 
  groupm.mleprobplot(mylarpoly.ld, 
                     distribution = "Lognormal",
                     relationship = "log")

summary(mylarpoly.lognor.groupm,
        stress.state = 0)

quantiles(mylarpoly.lognor.groupm, 
          new.data = 50,
          to = 0.2)

failure.probabilities(mylarpoly.lognor.groupm, 
                      new.data = 50)

plot(mylarpoly.lognor.groupm, 
     add.density.at = 50,
     xlim = c(31,400), 
     ylim=c(.1,10^8))

plot(mylarsub.lognor.groupm,
     data.ld = mylarpoly.ld, 
     add.density.at = 50, 
     xlim = c(31,400), 
     ylim = c(.1,10^8))


## make a model plot with detailed control of the x axis

plot(mylarsub.lognor.groupm,
     data.ld = mylarpoly.ld,
     density.at = c(100.3,122.4,157.1,219,50),
     title.option = 'blank',
     xlab = 'kV/mm', 
     hw.xaxis = list(ticlab = c(" 40", "100", "200","300", "400"),
                     ticloc = c(" 40", " 50", " 60", " 70", " 80", " 90", "100", "120", "140", "160","180", "200", "250","300","350", "400")))

## ------------------------------------------------------------------------
ICDevice2.ld <- frame.to.ld(icdevice2, 
                            response.column = c(1,2),
                            censor.column = 3,
                            case.weight.column = 4,
                            x.column = 5,  
                            data.title = "New Technology Device ALT", 
                            xlabel = "Degrees C", 
                            time.units = "Hours")

groupi.mleprobplot(ICDevice2.ld, 
                   distribution = "Lognormal")

ICDevice02.groupm.lognor <- 
  groupm.mleprobplot(ICDevice2.ld,
                     distribution = "Lognormal",
                     relationship = "Arrhenius",
                     ci.list = 6)

plot(ICDevice02.groupm.lognor,
     censor.time = 2304, 
     quant.lines = c(.01,0.1, .5))

quantiles(ICDevice02.groupm.lognor, 
          new.data = 100,
          to = 0.2)

failure.probabilities(ICDevice02.groupm.lognor, 
                      new.data = 100, 
                      time.vec = c(500,1000))

ICDevice2.groupm.lognor.ea <- 
  groupm.mleprobplot(ICDevice2.ld,
                     distribution = "Lognormal",
                     relationship = "Arrhenius",
                     ci.list = 6,
                     new.data = 100,
                     theta.start = c(-10.2, 0.8, 0.5),
                     parameter.fixed = c(F, T, F))

quantiles(ICDevice2.groupm.lognor.ea,
          new.data = 100,
          to = 0.2)

failure.probabilities(ICDevice2.groupm.lognor.ea, 
                      new.data = 100,
                      time.vec = c(500,1000))

plot(ICDevice2.groupm.lognor.ea, 
     censor.time = 2304,
     quant.lines = c(.01,0.1, .5))

## ------------------------------------------------------------------------
Tantalum.ld <- frame.to.ld(tantalum, 
                           response.column = 1,
                           censor.column = 2, 
                           case.weight.column = 3, 
                           x.column = c(4, 5), 
                           data.title = "Tantalum Capacitor Data", 
                           time.units = "Hours", 
                           xlabel = c("Volts","DegreesC"))

summary(Tantalum.ld)

design.plot(Tantalum.ld)
text(35.5864, 93.0106,"4/1000",cex = 1.2)
text(40.6815, 93.0106,"4/200",cex = 1.2)
text(46.5611, 93.0106,"2/50",cex = 1.2)
text(51.5986, 93.0106,"4/53 ",cex = 1.2)
text(46.3883, 53.8499,"6/502",cex = 1.2)
text(56.9744, 53.8499,"1/50",cex = 1.2)
text(46.4459, 10.737,"1/175",cex = 1.2)
text(62.4743,  10.737,"18/174",cex = 1.2)
title(main = "Tantalum Accelerated Life Test  Setup")

groupi.Tantalum <- 
  groupi.mleprobplot(Tantalum.ld,
                     distribution = "Weibull",
                     group = c(1, 2))

summary(groupi.Tantalum)

Tantalum.groupm.weib <- 
  groupm.mleprobplot(Tantalum.ld, 
                     group.var = c(1,2),
                     distribution = "Weibull",
                     relationship  = c("log","Arrhenius"))

summary(Tantalum.groupm.weib,
        stress.state = 0)

## doing a conditional model plot
plot(Tantalum.groupm.weib, 
     fixed.other.values = "30",
     focus.variable = "celsius")

plot(Tantalum.groupm.weib, 
     focus.variable = "volts", 
     fixed.other.values = 40)

failure.probabilities(Tantalum.groupm.weib, 
                      new.data = "35;85" )

quantiles(Tantalum.groupm.weib, 
          new.data = "35;85" )

Tantalum.groupm.weib <- 
  groupm.mleprobplot(Tantalum.ld,
                     group.var = c(1,2), 
                     distribution = "Weibull", 
                     relationship = c("log","Arrhenius"), 
                     formula = Location ~ + g(volts) +  g(celsius) + g(volts):g(celsius))

## ---- eval=FALSE---------------------------------------------------------
#  classh.turn.ld <- frame.to.ld(classh,
#                                response.column = "Turn.hours",
#                                censor.column = "Turn.censor",
#                                x.columns = "Temp",
#                                data.title = "Class H Turn Failures")
#  
#  summary(classh.turn.ld)
#  
#  ## analyze the classh.turn failure-times
#  
#  censored.data.plot(classh.turn.ld,
#                     y.axis = "log",
#                     x.axis = "Arrhenius")
#  
#  classh.turn.groupi <-
#    groupi.mleprobplot(classh.turn.ld,
#                       distribution = "Lognormal")
#  
#  summary(classh.turn.groupi)
#  
#  classh.turn.groupm <-
#    groupm.mleprobplot(classh.turn.ld,
#                       distribution = "Lognormal",
#                       relationship = "Arrhenius")
#  
#  summary(classh.turn.groupm)
#  
#  plot(classh.turn.groupm)
#  plot(classh.turn.groupm,
#       density.at = c(180,190,200,220,240,260),
#       censor.time = 12000,
#       quant.lines = c(.01,.1,.5))
#  
#  quantiles(classh.turn.groupm,
#            new.data = 180)
#  
#  failure.probabilities(classh.turn.groupm,
#                        new.data = 180)
#  
#  ## get and analyze the ground failures
#  
#  classh.ground.ld <- frame.to.ld(classh,	
#                                  response.column = "Ground.hours",
#                                  censor.column = "Ground.censor",
#                                  x.columns = "Temp",
#                                  data.title = "Class H Ground Failures")
#  
#  summary(classh.ground.ld)
#  
#  ## analyze the classh.ground failure-times
#  
#  censored.data.plot(classh.ground.ld,
#                     y.axis = "log",
#                     relationships = "Arrhenius")
#  
#  classh.ground.groupi <-
#    groupi.mleprobplot(classh.ground.ld,
#                       distribution = "Lognormal")
#  
#  summary(classh.ground.groupi)
#  
#  classh.ground.groupm <-
#    groupm.mleprobplot(classh.ground.ld,
#                       distribution = "Lognormal",
#                       relationship = "Arrhenius")
#  
#  summary(classh.ground.groupm)
#  
#  plot(classh.ground.groupm)
#  
#  ## get and analyze the phase failures
#  
#  classh.phase.ld <- frame.to.ld(classh,
#                                 response.column = "Phase.hours",
#                                 censor.column = "Phase.censor",
#                                 x.columns = "Temp",
#                                 data.title = "Class H Phase Failures")
#  
#  print(classh.phase.ld)
#  
#  summary(classh.phase.ld)
#  
#  censored.data.plot(classh.phase.ld,
#                     y.axis = "log",
#                     relationships = "Arrhenius")
#  
#  classh.phase.groupi <-
#    groupi.mleprobplot(classh.phase.ld,
#                       distribution = "Lognormal")
#  
#  summary(classh.phase.groupi)
#  
#  classh.phase.groupm <-
#    groupm.mleprobplot(classh.phase.ld,
#                       distribution = "Lognormal",
#                       relationship = "Arrhenius")
#  
#  summary(classh.phase.groupm)
#  
#  plot(classh.phase.groupm)
#  
#  quantiles(classh.phase.groupm,
#            new.data = 180)
#  
#  failure.probabilities(classh.phase.groupm,
#                        new.data = 180)
#  
#  AlloyZ.groupm <-
#    groupm.mleprobplot(AlloyZ.ld,
#                       distribution = "Weibull",
#                       relationship = "Log")
#  
#  plot(AlloyZ.groupm,
#       density.at = c(20,50,100,200))

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
AdhesiveBond.Weibull.altpv <-
  get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
                                           relationship = "Arrhenius",
                                           accelvar.units = c("DegreesC"),
                                                                       time.units = "Days", 
                                                                       censor.time = 183,
                                                                       probs = c(.001), 
                                                                       accelvar = c(50),
                                                                       slope = 0.726, 
                                                                       beta = 1.667)

print(AdhesiveBond.Weibull.altpv)

## ------------------------------------------------------------------------
AdhesiveBond0.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(100,100,100),
                           censor.times = c(243,243,243))

AdhesiveBond1.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(78,98,120),
                           number.of.units = c(155,60,84),
                           censor.times = c(183,183,183))

AdhesiveBond2.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(212,88,0),
                           censor.times = c(243,243,243))

AdhesiveBond3.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
                           number.of.units = c(150,60,90),
                           censor.times = c(243,243,243))

## optimum plan

AdhesiveBond4.altplan <-
  get.alt.test.plan.direct(accel.variable.levels = c(95,120),
                           number.of.units = c(212,88),
                           censor.times = c(183,183))

print(AdhesiveBond1.altplan)

## ------------------------------------------------------------------------
AdhesiveBond2.frame <- data.frame(DegreesC = c(80,100,120),
                                  SampleSize = c(150,60,90),
                                  CensorTimes = c(243,243,243))

AdhesiveBond2.altplan <- 
  get.alt.test.plan.frame(AdhesiveBond2.frame,
                          levels.columns = "DegreesC", 
                          censor.column = "CensorTimes",
                          allocation.column = "SampleSize",
                          describe.string = "AdhesiveBond Test Plan")

print(AdhesiveBond2.altplan)

## ------------------------------------------------------------------------
plot(AdhesiveBond1.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

plot(AdhesiveBond2.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

plot(AdhesiveBond3.altplan,
     ALT.plan.values = AdhesiveBond.Weibull.altpv,
     use.condition = 50)

## ------------------------------------------------------------------------
ALT.vcv(AdhesiveBond1.altplan,
        ALT.plan.values = AdhesiveBond.Weibull.altpv)

evaluate(AdhesiveBond1.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         quantile.of.interest = 0.5,
         use.condition = 50)

names(AdhesiveBond1.altplan)
names(AdhesiveBond.Weibull.altpv)

## ------------------------------------------------------------------------
simulate(AdhesiveBond1.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         use.condition = 50)

unfold(AdhesiveBond1.altplan, 
       AdhesiveBond.Weibull.altpv, 
       use.condition = 50)

evaluate(AdhesiveBond1.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.5)

## ------------------------------------------------------------------------
simulate(AdhesiveBond2.altplan, 
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         show.detail.on = 5)

simulate(AdhesiveBond2.altplan,
         ALT.plan.values = AdhesiveBond.Weibull.altpv,
         number.sim = 100,
         use.condition = 50)

evaluate(AdhesiveBond2.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.1)

AdhesiveBond3.sim.out <- 
  simulate(AdhesiveBond3.altplan,
           ALT.plan.values = AdhesiveBond.Weibull.altpv,
           use.condition = 50)

ALT.plot.time.v.x(AdhesiveBond3.sim.out,
                  x.of.interest = c(45))

ALT.plot.FracFail.v.Time(AdhesiveBond3.sim.out,
                         x.of.interest = c(45))

evaluate(AdhesiveBond3.altplan, 
         AdhesiveBond.Weibull.altpv,
         use.condition = 50,
         quantile.of.interest = 0.1)

SMRD:::plot.vpm.vs.size(vpm.vec = c(100, 150, 200), 
                         size.vec = c(1, 2, 3))

SMRD:::plot.volt.vs.size(volts.vec = c(100, 200, 300), 
                          size.vec = c(1, 2, 3))

## ------------------------------------------------------------------------
DeviceA.altpv <- 
  get.alt.plan.values.from.slope.and.point(distribution = "Lognormal",
                                           slope = 0.63,
                                           relationship = "Arrhenius",
                                           time.units = "Hours",
                                           censor.time = 5000,
                                           probs = 0.5469, 
                                           accelvar = 60,
                                           sigma = 0.98,
                                           use.conditions = 10)

print(DeviceA.altpv)

DeviceA1.altplan <- 
  get.alt.test.plan.direct(accel.variable.levels = c(10,40,60,80),
                           number.of.units = c(30,100,20,15),
                           censor.time =  c(5000,5000,5000,5000))

print(DeviceA1.altplan)

simulate(DeviceA1.altplan,
         ALT.plan.values = DeviceA.altpv, 
         nsim = 100,
         use.condition = 10)

DeviceA2.altplan <- 
  get.alt.test.plan.direct(accel.variable.levels = c(10,40,50,60,70,80),
                           number.of.units = c(30,27,27,27,27,27),
                           censor.time = c(5000,5000,5000,5000,5000,5000))

print(DeviceA2.altplan)

simulate(DeviceA2.altplan,
         ALT.plan.values = DeviceA.altpv, 
         nsim = 100, 
         use.condition = 10)

## ---- eval=FALSE---------------------------------------------------------
#  ## generate ALT plans
#  
#  hold.altplan("Weibull",
#               a = -4,
#               b1 = -10,
#               perc = 0.1,
#               iopta = 3,
#               iopts = 2,
#               pifix = 0)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
Resistor.rmd <- frame.to.rmd(resistor,
                             response.column = "percent",
                             time.column = "hours", 
                             unit.column = "resistor",
                             data.title = "Carbon-Film Resistor Accelerated Test", 
                             response.units = "Percent Increase in Resistance", 
                             x.columns = "celsius" )

## plot the degradation data

plot(Resistor.rmd)

plot(Resistor.rmd, y.axis = "log")

plot(Resistor.rmd, y.axis = "sqrt")

plot(Resistor.rmd, x.axis = "log")

plot(Resistor.rmd, 
     x.axis = "log",
     y.axis = "log",
     group.var = NA)

names(Resistor.rmd)

Resistor.ld1 <- rmd.to.ld(Resistor.rmd, 
                          fail.level = 5, 
                          subset = "hours > 0.6",
                          censor.time = 35,
                          x.axis = "sqrt")

SMRD:::plot.rmd.average(Resistor.rmd) #issue
SMRD:::plot.rmd.residual(Resistor.ld1) #issue

Resistor.ld <- rmd.to.ld(Resistor.rmd, 
                         fail.level = 5, 
                         subset = "hours" > 0.6,
                         censor.time = 35)

## ------------------------------------------------------------------------
SMRD:::plot.rmd.residual(Resistor.ld)

trellis.plot(Resistor.rmd,
             order.groups = F,
             outer.plot = F)

trellis.plot(Resistor.rmd,
             order.groups = F,
             outer.plot = T)

## summarize the data

print(Resistor.ld) 
summary(Resistor.ld)

## analyze the Resistor pseudo failure-time data

censored.data.plot(Resistor.ld, 
                   x.axis = "log",
                   y.axis  = "Arrhenius")

groupi.mleprobplot(Resistor.ld, distribution = "Lognormal")

Resistor.groupm.out <- groupm.mleprobplot(Resistor.ld, 
                                          distribution = "Lognormal", 
                                          relationship  = "Arrhenius",
                                          ci.list = 1)

plot(Resistor.groupm.out,
     censor.time = 8000)

## ------------------------------------------------------------------------
MetalWear.rmd <- frame.to.rmd(metalwear,
                              response.column = 1,
                              time.column = 3,
                              unit.column = 2,
                              data.title = "Sliding Metal Wear",
                              x.columns = 4,
                              skip = 1)

plot(MetalWear.rmd)

plot(MetalWear.rmd, 
     x.axis="log",
     y.axis="log")

## ------------------------------------------------------------------------
MetalWear.ld <- rmd.to.ld(MetalWear.rmd,
                          fail.level = 50,
                          ylim = c(2,100),
                          xlim = c(2,1000),
                          x.axis = "log",
                          y.axis = "log")

censored.data.plot(MetalWear.ld)

censored.data.plot(MetalWear.ld,
                   y.axis = "log", 
                   xlab = "Grams", 
                   ylab = "Cycles")

MetalWear.groupi.out <- groupi.mleprobplot(MetalWear.ld,
                                           distribution = "Lognormal")

MetalWear.groupm.out <- groupm.mleprobplot(MetalWear.ld,
                                           distribution = "lognormal",
                                           relationship = "class",
                                           ci.list = 1)

MetalWear.groupm.out <- groupm.mleprobplot(MetalWear.ld,
                                           distribution = "lognormal", 
                                           relationship = "linear", 
                                           ci.list = 1)

plot(MetalWear.groupm.out, 
     censor.time = 500)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ------------------------------------------------------------------------
## create the ddd data object
Insulation.ddd <- frame.to.ddd(insulation,
                               response.column = 3, 
                               time.column = 1,
                               x.columns = 2,
                               data.title = "Voltage Breakdown Data",
                               response.units = "Volts",
                               time.units = "Weeks")

print(Insulation.ddd)

## ------------------------------------------------------------------------
plot(Insulation.ddd,
     transformation.Response = "log",
     transformation.time = "linear")

tmp <- groupi.Dest.Degrad.indivplots(Insulation.ddd,
                                     transformation.response = "log", 
                                     transformation.time = "linear",
                                     distribution = "normal")

groupi.Dest.Degrad.oneplot(Insulation.ddd,
                           transformation.response = "log", 
                           transformation.time = "linear",
                           distribution="normal")

## ------------------------------------------------------------------------
groupm.Dest.Degrad(Insulation.ddd, 
                   distribution = "normal",
                   transformation.response = "log10",
                   transformation.x = "invtemp",
                   transformation.time = "linear")


groupm.Dest.Degrad(Insulation.ddd, 
                   distribution = "normal",
                   transformation.response = "log",
                   transformation.x = "arrhenius",
                   transformation.time = "linear")

## ------------------------------------------------------------------------
Insulation.groupi.Dest.Degrad <- 
  groupi.Dest.Degrad(Insulation.ddd,
                     distribution = "normal",
                     transformation.response = "log",
                     transformation.time = "sqrt")

plot(Insulation.groupi.Dest.Degrad,
     transformation.x = "Arrhenius")

## ------------------------------------------------------------------------
Insulation.groupm.Dest.Degrad <-
  groupm.Dest.Degrad(Insulation.ddd,
                     distribution = "normal", 
                     transformation.response = "log",
                     transformation.x = "arrhenius", 
                     transformation.time = "sqrt")

Insulation.groupm.Dest.Degrad <- 
  groupm.Dest.Degrad(Insulation.ddd,
                     distribution = "normal",
                     transformation.response = "log",
                     transformation.x = "arrhenius",
                     transformation.time = "sqrt",
                     new.data = c("150,260"))

residual.plots(Insulation.groupm.Dest.Degrad)

## ---- echo=FALSE---------------------------------------------------------

library(SMRD)

## ---- eval=FALSE---------------------------------------------------------
#  SMRDOptions(SMRD.DebugLevel = "development")

## ------------------------------------------------------------------------
InsulationBrkdwn.ADDTplan <- 
  get.allocation.matrix(list(DegreesC = c(180,225,250,275)),
                        times = c(1,2,4,8,16,32,48,64),
                        time.units = "Weeks",
                        reps = 4)

plot(InsulationBrkdwn.ADDTplan)

InsulationBrkdwn.ADDTpv <- 
  get.ADDT.plan.values(distribution = "normal",
                       transformation.x = "Arrhenius",
                       transformation.response = "log", 
                       transformation.time = "linear",
                       beta0 = 2.58850162033243,
                       beta1 = -476873415881.376,
                       beta2 = 1.41806367703643,
                       sigma = 0.172609,
                       time.units = "Weeks",
                       response.units = "Volts", 
                       FailLevel = 10,
                       use.condition = 100)

print(InsulationBrkdwn.ADDTpv)

InsulationBrkdwn.vADDTplan <-
  hframe.to.vframe(InsulationBrkdwn.ADDTplan)

sum(allocation(InsulationBrkdwn.vADDTplan))

names(InsulationBrkdwn.ADDTpv)

InsulationBrkdwn.plan.sim.out <- 
  simulate(InsulationBrkdwn.ADDTplan,
           nsim = 5,
           ADDT.plan.values = InsulationBrkdwn.ADDTpv)

ADDT.plot.time.v.x(InsulationBrkdwn.plan.sim.out)

ADDT.plot.Deg.v.Time(InsulationBrkdwn.plan.sim.out)
ADDT.plot.FracFail.v.Time(InsulationBrkdwn.plan.sim.out)

ADDT.vcv(InsulationBrkdwn.ADDTpv,
         hframe.to.vframe(InsulationBrkdwn.ADDTplan))

evaluate(InsulationBrkdwn.ADDTplan,
         ADDT.plan.values = InsulationBrkdwn.ADDTpv,
         use.condition = "150",
         FailLevel = 10,
         quantile.of.interest = c(0.1,.2,.3,.4,.9))

evaluate(InsulationBrkdwn.ADDTplan,
         ADDT.plan.values = InsulationBrkdwn.ADDTpv,
         use.condition = "150",
         FailLevel = 2,
         quantile.of.interest = c(.05,0.1))

evaluate(InsulationBrkdwn.ADDTplan,
         InsulationBrkdwn.ADDTpv,
         use.condition = "150",
         FailLevel = 10,
         quantile.of.interest = 0.2)

plot(InsulationBrkdwn.ADDTplan,
     ADDT.plan.values = InsulationBrkdwn.ADDTpv,
     use.condition = "150",
     FailLevel = 10,
     quantile.of.interest = 0.2)

##	Large-Sample example as a check

InsulationBrkdwn.test.ADDTplan <- 
  get.allocation.matrix(list(DegreesC = c(180,225,250,275)),
                        times = c(1,2,4,8,16,32,48,64),
                        time.units = "Weeks",
                        reps = 400)

print(InsulationBrkdwn.test.ADDTplan)
plot(InsulationBrkdwn.test.ADDTplan)

#     Interpretation parameter MLEs
#      beta0       beta1 beta2  sigma
#      2.589 -4.769e+011 1.418 0.1726

## ------------------------------------------------------------------------
InsulationBrkdwn.test.ADDTpv <-
  get.ADDT.plan.values(distribution = "normal",
                       transformation.x = "Arrhenius",
                       transformation.response = "log",
                       transformation.time = "linear",
                       beta0 = 2.58850162033243,
                       beta1 = -476873415881.376,
                       beta2 = 1.41806367703643,
                       sigma = 0.172609,
                       time.units = "Weeks",
                       response.units = "Volts",
                       use.condition = 50)

ADDT.vcv(ADDT.plan.values = InsulationBrkdwn.ADDTpv,
         ADDT.test.plan = hframe.to.vframe(InsulationBrkdwn.ADDTplan))

evaluate(InsulationBrkdwn.test.ADDTplan,
         ADDT.plan.values = InsulationBrkdwn.test.ADDTpv,
         use.condition = "150",
         FailLevel = 10,
         quantile.of.interest = c(0.1,.2,.3,.4,.9))

evaluate(InsulationBrkdwn.test.ADDTplan,
         ADDT.plan.values = InsulationBrkdwn.test.ADDTpv,
         use.condition = "150",
         FailLevel = 2,
         quantile.of.interest = c(.05,0.1))

evaluate(InsulationBrkdwn.test.ADDTplan,
         ADDT.plan.values = InsulationBrkdwn.test.ADDTpv,
         use.condition = "150",
         FailLevel = 10,
         quantile.of.interest = c(.2,.3,.4,.9))

BondTest.ADDTplan <-
  get.allocation.matrix(list(DegreesC = c(50,80,100,120)),
                        times = c(1,2,4,8,16,32,48,64),
                        time.units = "Weeks",
                        reps = 10)

Filament.ADDTplan <- 
  get.allocation.matrix(list(volts = c(5,10),DegreesC = c(40,50,60)),
                        times = c(1,2,5,10,20,50),
                        time.units = "Weeks")

# Filament.hframe.ADDTplan  <-
#   get.ADDT.test.plan.hframe(filament,
#                             levels.columns = c(1,2),
#                             time.columns = 3:7)

StrengthCompare.ADDTplan <- 
  get.allocation.matrix(list(DegreesC = c(50,60,70),
                             Type = c("Type1","Type2")),
                        times = c(1,2,5,10,20,50),
                        time.units = "Weeks")

## summarize simulation results
marginalize.sim(InsulationBrkdwn.plan.sim.out, 
                focus.quantity = "failure probability",
                focus.quantity.detail = 14914.9,
                x.of.interest = "150",
                FailLevel = 4)

SMRD:::plot.marginals.sim(InsulationBrkdwn.plan.sim.out,
                           focus.quantity = "parameter",
                           focus.quantity.detail = 1,
                           FailLevel = 4)

SMRD:::plot.marginals.sim(InsulationBrkdwn.plan.sim.out,
                           focus.quantity = "parameter",
                           focus.quantity.detail = 1,
                           plot.type = "density",
                           FailLevel = 4)

SMRD:::plot.marginals.sim(InsulationBrkdwn.plan.sim.out,
                           focus.quantity = "quantile",
                           focus.quantity.detail = 0.1,
                           x.of.interest = "150",
                           plot.type = "density",
                           FailLevel = 4)

SMRD:::plot.joint.sim(InsulationBrkdwn.plan.sim.out, 
                       focus.quantity1 = "quantile",
                       focus.quantity.detail1 = 0.1,
                       x.of.interest1 = "150",
                       focus.quantity2 = "parameter",
                       focus.quantity.detail2 = 3,
                       x.of.interest2 = NA,
                       FailLevel = 4)

SMRD:::plot.joint.and.marginals.sim(InsulationBrkdwn.plan.sim.out, 
                                     focus.quantity1 = "quantile",
                                     focus.quantity.detail1 = 0.1,
                                     x.of.interest1 = "150",
                                     focus.quantity2 = "parameter",
                                     focus.quantity.detail2 = 3,
                                     x.of.interest2 = NA,
                                     FailLevel = 4)

summarize.simultation.results(InsulationBrkdwn.plan.sim.out, 
                              "Marginal only", 
                              focus.quantity1 = "quantile",
                              focus.quantity.detail1 = 0.1,
                              x.of.interest1 = "150",
                              focus.quantity2 = "parameter",
                              focus.quantity.detail2 = 3,
                              x.of.interest2 = NA,
                              FailLevel = 4)

summarize.simultation.results(InsulationBrkdwn.plan.sim.out, 
                              "Marginal only", 
                              focus.quantity1 = "quantile",
                              focus.quantity.detail1 = 0.1,
                              x.of.interest1 = "150",
                              focus.quantity2 = "parameter",
                              focus.quantity.detail2 = 3,
                              x.of.interest2 = NA,
                              plot.type = "density",
                              FailLevel = 4)

summarize.simultation.results(InsulationBrkdwn.plan.sim.out,
                              "Joint and Marginal",
                              focus.quantity1 = "quantile",
                              focus.quantity.detail1 = 0.1,
                              x.of.interest1 = "150",
                              focus.quantity2 = "parameter",
                              focus.quantity.detail2 = 3,
                              x.of.interest2 = NA,
                              plot.type = "density",
                              FailLevel = 4)

summarize.simultation.results(InsulationBrkdwn.plan.sim.out, 
                              "Joint only", 
                              focus.quantity1 = "quantile",
                              focus.quantity.detail1 = 0.1,
                              x.of.interest1 = "150",
                              focus.quantity2 = "parameter",
                              focus.quantity.detail2 = 3,
                              x.of.interest2 = NA,
                              FailLevel = 4)

## ------------------------------------------------------------------------
##	AdhesiveBondB Example
AdhesiveBondB.SEV.ADDTpv <- 
  get.ADDT.plan.values(distribution = "normal",
                       transformation.x = c("Arrhenius"),
                       transformation.response = "log",
                       transformation.time = "Square root",
                       beta0 = 4.471,
                       the.slope = -0.1025,
                       slope.at = c(50),
                       beta2 = c(0.6364),
                       sigma = 0.158,
                       time.units = "Weeks",
                       response.units = "Newtons",
                       FailLevel = 40,
                       use.condition = "25")

AdhesiveBondB.ADDTplan <- 
  get.allocation.matrix(list(DegreesC = c(25,50,60,70)),
                        times = c(0,2,4,6,12,16),
                        time.units = "Weeks",
                        reps = 6)

ADDT.vcv(ADDT.plan.values = AdhesiveBondB.SEV.ADDTpv,
         ADDT.test.plan = hframe.to.vframe(AdhesiveBondB.ADDTplan))

last.out <- evaluate(AdhesiveBondB.ADDTplan,
                     ADDT.plan.values = AdhesiveBondB.SEV.ADDTpv, 
                     quantile.of.interest = c(.1,.5,.9),
                     use.condition = 25,
                     FailLevel = 40)

## two variable example

AdhesiveBondC.ADDTpv <-
  get.ADDT.plan.values(distribution = "sev",
                       transformation.x = c("Arrhenius","Humidity"),
                       transformation.response = "log",
                       transformation.time = "Square root",
                       beta0 = 2.168,
                       the.slope = -0.00709030595,
                       slope.at = c(30,50),
                       beta2 = c(0.6666,.2),
                       sigma = 0.1807,
                       time.units = "Weeks",
                       response.units = "Pounds",
                       FailLevel = 2,
                       use.condition = "30;50")

AdhesiveBondC.ADDTplan <-
  get.allocation.matrix(list(DegreesC = c(40,50,60),RH = c(20,80)),
                        times = c(1,2,5,10,20,50),
                        time.units = "Weeks",
                        reps = 6)

tmp.pmodel <- 
  pseudo.model(ADDT.plan.values = AdhesiveBondC.ADDTpv,
               ADDT.test.plan = hframe.to.vframe(AdhesiveBondC.ADDTplan))

f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, 
                   tmp.pmodel)

f.ADDT.origparam(f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, tmp.pmodel), tmp.pmodel)

## f.analorigparmvcv exercised in the following

ADDT.vcv(ADDT.plan.values = AdhesiveBondC.ADDTpv,
         ADDT.test.plan = hframe.to.vframe(AdhesiveBondC.ADDTplan))

evaluate(AdhesiveBondC.ADDTplan,
         ADDT.plan.values = AdhesiveBondC.ADDTpv,
         quantile.of.interest = c(0.1,.2,.3,.4,.9))

plot(AdhesiveBondC.ADDTplan,
     ADDT.plan.values = AdhesiveBondC.ADDTpv,
     quantile.of.interest = 0.2)

AdhesiveBondC.vframe <- hframe.to.vframe(AdhesiveBondC.ADDTplan)

