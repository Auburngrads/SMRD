PlotRMDQuantileEstimates <-
function (rmd.fit, p, print.stuff = F)
{
    the.results <- DegSummary(rmd.fit)
    PassedGroupedData <- attr(rmd.fit, "data.rmd")
    time.vec <- PassedGroupedData$Time
    unique.units <- unique(as.character(PassedGroupedData$Unit))
    unique.times <- unique(time.vec)
    if (print.stuff)
        print(DegSummary(rmd.fit)$parameters)
    attr(rmd.fit, "data.rmd") <- get(envir = .frame0,  "RMD.data.grouped")
    Time <- numeric(0)
    Unit <- character(0)
    TimeRange <- range(unique.times)
    for (i in 1:length(unique.units)) {
        Time <- c(Time, unique.times)
        Unit <- c(Unit, rep(unique.units[i], length(unique.times)))
    }
    the.newdata <- data.frame(Time = Time, Unit = Unit)
    rmd.predicted <- predict(rmd.fit, newdata = the.newdata)
    names.rmd.predicted <- names(rmd.predicted)
    plot.paper(range(time.vec), range(rmd.predicted), grids = F,
        xlab = "Time", ylab = "Degradation")
    for (i in 1:length(unique.units)) {
        the.predicted <- rmd.predicted[names.rmd.predicted ==
            unique.units[i]]
        lines(unique.times, the.predicted)
    }
    if (!is.na(p) && !missing(p)) {
        cat("Beginning quantile results")
        quantile.results <- quantile.rmd(p, rmd.fit, time.vec = unique.times)
        cat("Done quantile results")
        lines(unique.times, quantile.results, lwd = 5, col = 3)
        title(paste("Predicted Paths for Individual Units \nand MLE for the ",
            p, "Quantile for the Population"))
    }
    else {
        title("Predicted Paths for Individual Units")
    }
}
