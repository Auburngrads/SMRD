FitLinearDegradationData <-
function (PassedData, print.stuff = T, plot.stuff = T, observation.time.range = NA,
    maxIter = 20000, msMaxIter = 20000, msVerbose = TRUE, niterEM = 20000)
{
    PassedGroupedData <- groupedData(Degradation ~ Time | Unit,
        data = PassedData)
    time.units <- get.time.units(PassedData)
    degradation.units <- attr(PassedData, "degradation.units")
    if (!any(is.na(observation.time.range))) {
        Times <- PassedGroupedData$Time
        keep <- Times >= observation.time.range[1] & Times <=
            observation.time.range[2]
        PassedGroupedData <- PassedGroupedData[keep, ]
        if (plot.stuff)
            PlotData.rmd(PassedGroupedData)
    }
    assign(envir = .frame0,  inherits = TRUE,"RMD.data.grouped", PassedGroupedData)
    control.list <- lmeControl(maxIter = maxIter, msMaxIter = msMaxIter,
        msVerbose = msVerbose, niterEM = niterEM)
    rmd.fit <- lme(RMD.data.grouped, random = ~Time, control = control.list)
    if (print.stuff)
        print(rmd.fit)
    the.results <- DegSummary(rmd.fit)
    time.vec <- PassedGroupedData$Time
    unique.units <- unique(as.character(PassedGroupedData$Unit))
    unique.times <- unique(time.vec)
    if (print.stuff)
        print(DegSummary(rmd.fit)$parameters)
    attr(rmd.fit, "data.rmd") <- get(envir = .frame0,  "RMD.data.grouped")
    invisible(rmd.fit)
}
