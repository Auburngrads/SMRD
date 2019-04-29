PlotCumulativePredictTable <-
function (theCumulativePredictTable, lty = 1, lwd = 1, add = FALSE, 
    plot.what = "density", plot.points = FALSE, my.title = NULL, 
    xlim = c(NA, NA), ylim = c(NA, NA)) 
{
    Time <- theCumulativePredictTable[, "Time"]
    Efail <- theCumulativePredictTable[, "Efail"]
    warranty.time <- attr(theCumulativePredictTable, "warranty.time")
    mlest.out <- attr(theCumulativePredictTable, "mlest.out")
    data.ld <- mlest.out$data.ld
    data.title <- attr(data.ld, "data.title")
    time.units <- attr(data.ld, "time.units")
    distribution <- mlest.out$distribution
    switch(plot.what, density = {
        if (!add) {
            xrna <- is.na(xlim)
            if (any(xrna)) xlim[xrna] <- range(Time)[xrna]
            yrna <- is.na(ylim)
            if (any(yrna)) ylim[yrna] <- range(Efail)[yrna]
            plot.paper(xlim, ylim, xlab = time.units)
            if (is.null(my.title)) my.title <- paste(data.title, 
                distribution, "Distribution", "\nIncremental Returns Over Time")
            title(main = my.title)
        }
        lines(Time, Efail, lty = lty, lwd = lwd)
        if (plot.points) points(Time, Efail)
    }, cumulative = {
        CumEfail <- cumsum(Efail)
        if (!add) {
            xrna <- is.na(xlim)
            if (any(xrna)) xlim[xrna] <- range(Time)[xrna]
            yrna <- is.na(ylim)
            if (any(yrna)) ylim[yrna] <- range(CumEfail)[yrna]
            plot.paper(xlim, ylim, xlab = time.units)
            if (is.null(my.title)) my.title <- paste(data.title, 
                distribution, "Distribution", "\nCumulative Returns Over Time")
            title(main = my.title)
        }
        lines(Time, CumEfail, lty = lty, lwd = lwd)
    }, {
        warning(paste(plot.what, "not recognized as a valid option"))
    })
}
