#' @export
plot.rmd.predicted <-
function (x, p = NA, col.quantile.mle = 3, plot.data.points = T,
    col.predicted = 4,...)
{
    PassedGroupedData <- attr(x, "data.rmd")
    time.units <- get.time.units(PassedGroupedData)
    degradation.units <- attr(PassedGroupedData, "degradation.units")
    Time <- PassedGroupedData$Time
    Degradation <- PassedGroupedData$Degradation
    Unit <- PassedGroupedData$Unit
    the.results <- DegSummary(x)
    Degradation <- PassedGroupedData$Degradation
    time.vec <- PassedGroupedData$Time
    unique.units <- unique(as.character(PassedGroupedData$Unit))
    unique.times <- unique(time.vec)
    TimeOrder <- numeric(0)
    UnitOrder <- character(0)
    TimeRange <- range(unique.times)
    for (i in 1:length(unique.units)) {
        TimeOrder <- c(TimeOrder, unique.times)
        UnitOrder <- c(UnitOrder, rep(unique.units[i], length(unique.times)))
    }
    the.newdata <- data.frame(Time = TimeOrder, Unit = UnitOrder)
    rmd.predicted <- predict(x, newdata = the.newdata)
    names.rmd.predicted <- names(rmd.predicted)
    plot.paper(range(time.vec), range(rmd.predicted), grids = F,
        xlab = time.units, ylab = degradation.units)
    for (i in 1:length(unique.units)) {
        the.predicted <- rmd.predicted[names.rmd.predicted ==
            unique.units[i]]
        lines(unique.times, the.predicted, col = col.predicted)
        if (plot.data.points) {
            the.ones <- unique.units[i] == Unit
            sub.times <- Time[the.ones]
            sub.degrad <- Degradation[the.ones]
            lines(sub.times, sub.degrad, type = "b")
        }
    }
    if (!is.na(p)) {
        cat("Beginning quantile results\n")
        quantile.results <- quantile.rmd(p, x, time.vec = unique.times)
        cat("Done quantile results")
        lines(unique.times, quantile.results, lwd = 5, col = col.quantile.mle)
        mtext(paste(get.data.title(PassedGroupedData), "\n",
            "Fitted (Predicted) Paths for Individual Units \nand MLEs for the ",
            p, "Quantile for the Population vs Time"), line = 3,
            cex = 1.2)
    }
    else {
        title("Predicted Paths for Individual Units")
    }
}
