#' Title
#'
#' @param data1.rdu 
#' @param data2.rdu 
#' @param conf.level 
#' @param plot.data 
#' @param title.option 
#' @param my.title 
#' @param xlab 
#' @param ylab 
#' @param plot.seg 
#' @param lty 
#' @param xlim 
#' @param ylim 
#' @param band.type 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Grids1.rdu <- frame.to.rdu(grids1,
#'                            ID.column = "unit", 
#'                            time.column = "days",
#'                            event.column = "event",
#'                            data.title = "Grids1 Replacement Data",
#'                            time.units = "Days")
#' 
#' summary(Grids1.rdu)
#' 
#' event.plot(Grids1.rdu)
#' print(mcf(Grids1.rdu))
#' mcf.plot(Grids1.rdu)
#' 
#' Grids2.rdu <- frame.to.rdu(grids2, 
#'                            time.column = c(2), 
#'                            event.column = 3,
#'                            ID.column = 1, 
#'                            data.title = "Grids2 Replacement Data",
#'                            time.units ="Days")
#' 
#' summary(Grids2.rdu)
#' 
#' #attr(Grids2.rdu,"WindowInfo")
#' 
#' PlotMCFandNHPP(Grids2.rdu,
#'                form = "power rule")
#' Grids2.mcf <- mcf(Grids2.rdu)
#' plot(Grids2.mcf)
#' 
#' event.plot(Grids2.rdu)
#' print(mcf(Grids2.rdu))
#' mcf.plot(Grids2.rdu)
#' 
#' mcf.diff.plot(Grids1.rdu,
#'               Grids2.rdu,
#'               plot.seg = T,
#'               xlab = "Locomotive Age in Days",
#'               ylab = "Difference in Mean Cumulative Replacements")
#' 
#' }
mcf.diff.plot <-
function (data1.rdu, data2.rdu, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
    plot.data = F, title.option = GetSMRDDefault("SMRD.TitleOption"), my.title = NULL, xlab = paste("Time in", 
        get.time.units(data1.rdu)), ylab = "Difference in Mean Cumulative Failures", 
    plot.seg = T, lty = 1, xlim = c(NA, NA), ylim = c(NA, 
        NA), band.type = "p") 
{
    mcf.diff.out <- mcf.diff(data1.rdu, data2.rdu, conf.level = conf.level)
    maxctime <- mcf.diff.out$maxcen
    xmudiffhat <- mcf.diff.out$xmudiffhat
    par(mar = c(5.1, 6.5, 4.1, 2.1))
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(c(mcf.diff.out$utime, maxctime))[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(c(mcf.diff.out$lcl, mcf.diff.out$ucl))[yrna]
    plot(xlim, ylim, type = "n", xlab = "", ylab = "", 
        cex = 1.5, las = 1)
    title(xlab = xlab, cex = 1.5)
    title(ylab = ylab, cex = 1.5, mgp = c(5, 1, 0))
    if (is.null(my.title)) 
        my.title <- paste("Comparison of Mean Cumulative Functions")
    if (title.option == "full") {
        mtext(text = my.title, side = 3, line = 3, cex = 1.5)
        my.title2 <- paste(get.data.title(data1.rdu), "MCF minus", 
            get.data.title(data2.rdu), "MCF")
        mtext(text = my.title2, side = 3, line = 1)
    }
    if (plot.seg) {
        mcf.segments(mcf.diff.out$utime, xmudiffhat, maxctime, 
            lty = lty, lwd = 3)
        if (conf.level > 0 && band.type == "p") {
            mcf.segments(mcf.diff.out$utime, mcf.diff.out$lcl, 
                maxctime, lty = 2, lwd = 3)
            mcf.segments(mcf.diff.out$utime, mcf.diff.out$ucl, 
                maxctime, lty = 2, lwd = 3)
        }
    }
    else {
        mcf.lines(mcf.diff.out$utime, xmudiffhat, maxctime, lty = 1, 
            lwd = 3)
        if (conf.level > 0) {
            mcf.lines(mcf.diff.out$utime, mcf.diff.out$lcl, maxctime, 
                lty = 2, lwd = 2)
            mcf.lines(mcf.diff.out$utime, mcf.diff.out$ucl, maxctime, 
                lty = 2, lwd = 2)
        }
    }
    abline(h = 0)
    the.table <- cbind(mcf.diff.out$utime, mcf.diff.out$xmudiffhat, 
        mcf.diff.out$se.xmudiffhat, mcf.diff.out$lcl, mcf.diff.out$ucl)
    dimnames(the.table) <- list(rep(" ", nrow(the.table)), c("Time", 
        "MuDiff-hat", "SD MuDiff-hat", "Lower", "Upper"))
    return(the.table)
    invisible()
}
