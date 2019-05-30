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

