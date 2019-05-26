## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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
print(ShockAbsorber.mlest)$ll.text
print(ShockAbsorber.mlest)$mttf.text
print(ShockAbsorber.mlest)$mle.table
print(ShockAbsorber.mlest)$vcv.matrix
print(ShockAbsorber.mlest)$param.corr.matrix
print(ShockAbsorber.mlest)$failure.probabilities
print(ShockAbsorber.mlest)$quantiles

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

