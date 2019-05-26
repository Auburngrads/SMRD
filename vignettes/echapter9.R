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

