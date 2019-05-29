## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

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
                  my.title="")

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
legend(SMRD2:::x.loc(.01),
       SMRD2:::y.loc(.95),
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

