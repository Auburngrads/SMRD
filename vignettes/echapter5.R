## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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

