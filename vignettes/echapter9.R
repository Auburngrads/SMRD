## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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

SMRD2:::compare.summary.boot.npar.npar.out(ShockAbsorber.boot.np)

## ------------------------------------------------------------------------
ShockAbsorber.boot.np2 <- nonparametric.bootstrap(ShockAbsorber.ld,
                                                  number.sim = 20)

plot(ShockAbsorber.boot.np2)

summary(ShockAbsorber.boot.np2)

SMRD2:::compare.summary.boot.npar.npar.out(ShockAbsorber.boot.np2)

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

SMRD2:::compare.summary.boot.npar.npar.out(BearingCage.boot.np)

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

SMRD2:::compare.summary.boot.npar.npar.out(bulb.boot.np,
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

