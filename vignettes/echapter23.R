## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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

