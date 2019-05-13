#' Title
#'
#' @param theCumulativePredictTable 
#' @param lty 
#' @param lwd 
#' @param add 
#' @param plot.what 
#' @param plot.points 
#' @param my.title 
#' @param xlim 
#' @param ylim 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # No warranty enforcement
#' 
#' DeviceN.NoEnforce.weib <-
#'   CumulativePredictTable(DeviceN.mlest.weib,
#'                          time.increment = 1,
#'                          number.time.units.ahead = 50,
#'                          warranty.time = 1000)
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "density")
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "cumulative")
#' 
#' # Strict warranty enforcement
#' 
#' DeviceN.Enforced.weib <-
#'   CumulativePredictTable(DeviceN.mlest.weib,
#'                          time.increment = 1,
#'                          number.time.units.ahead = 50,
#'                          warranty.time = 36)
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "density")
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "cumulative")
#' # Strict vs. no warranty enforcement (Weibull distribution)
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "density",
#'                            ylim = c(0,NA))
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "density",
#'                            add = TRUE,
#'                            lty = 3,
#'                            lwd = 3)
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "cumulative")
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "cumulative",
#'                            add = TRUE,
#'                            lty = 3,
#'                            lwd = 3)
#' }
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
