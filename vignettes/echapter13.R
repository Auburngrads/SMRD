## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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
GaAsLaser.ld <- SMRD2:::rmd.to.ld(GaAsLaser.rmd, 
                          fail.level = 10, 
                          x.axis = "sqrt")

SMRD2:::plot.rmd.residual(GaAsLaser.ld)

GaAsLaser.ld <- SMRD2:::rmd.to.ld(GaAsLaser.rmd,
                          fail.level = 10)

SMRD2:::plot.rmd.residual(GaAsLaser.ld)

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

