#' Title
#'
#' @param grid.list 
#' @param which 
#' @param lty 
#' @param col 
#' @param do.legend 
#' @param do.list 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Snubber.ld <- frame.to.ld(snubber, 
#'                           response.column = "cycles", 
#'                           censor.column = "event",
#'                           time.units = "Cycles",
#'                           case.weight.column = "count",
#'                           x.columns = "design")
#' 
#' event.plot(Snubber.ld)
#' summary(Snubber.ld)
#' 
#' Snubber.groupi.nor.out <-  groupi.mleprobplot(Snubber.ld,"normal")
#' 
#' tmpxx <- groupi.contour(Snubber.ld,
#'                         "Weibull",
#'                         the.quantile = 0.1)
#' 
#' tmpxx <- groupi.contour(Snubber.ld,
#'                         "lognormal",
#'                         the.quantile = 0.1)
#' 
#' multiple.profile.plot(tmpxx, which = "x")
#' multiple.profile.plot(tmpxx, which = "y")
#' 
#' }
multiple.profile.plot <-
function (grid.list, 
          which, 
          lty = (1:(length(grid.list) + 1))[-2],
          col = 1:length(grid.list), 
          do.legend = "On plot",
          do.list = names(grid.list))
{
    xlim <- NULL
    for (i in 1:length(grid.list)) {
        xlim <- range(xlim, grid.list[[i]][[which]])
    }
    for (i in 1:length(grid.list)) {
        if (i == 1) {
            add <- F
}       else {
            add <- T
            xlim <- NULL
        }
        profile.plot(profile.grid(grid.list[[i]], which = which),
            add = add, lty = lty[i], col = col[i], lwd = 2, xlim = xlim)
    }
    if (do.legend == "On plot")
        legend(x.loc(0.003), y.loc(0.996), do.list, cex = 1.1,
            bty = "n", col = col, lty = lty, y.intersp = 0.675)
    if (do.legend == "New page" || do.legend == "New file") {
        if (do.legend == "New file")
            postscript(file = "Save_legend.ps", horizontal = T)
        plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
            xaxt = "n", yaxt = "n")
        legend(x.loc(0.003), y.loc(0.997), do.list, cex = 1.1,
            bty = "n", col = col, lty = lty, y.intersp = 0.675)
        if (do.legend == "New file")
            dev.off()
    }
    invisible()
}
