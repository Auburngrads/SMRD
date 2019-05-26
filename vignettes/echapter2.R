## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ---- fig.width=7, fig.height=5------------------------------------------
distribution.plot("Weibull",
                  shape = c(1.7),
                  prob.range = c(.000001,.99))

