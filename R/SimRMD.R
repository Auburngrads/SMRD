SimRMD <-
function (RMD.pv, RMD.plan, observation.time.range = NA, plot.stuff = T,
    print.stuff = T)
{
    RMD.data <- sim.RMD.Data(RMD.pv, RMD.plan, plot.stuff = plot.stuff,
        print.stuff = print.stuff)
    rmd.fit.out <- FitLinearDegradationData(RMD.data, plot.stuff = plot.stuff,
        print.stuff = print.stuff, observation.time.range = observation.time.range)
    return(rmd.fit.out)
}
