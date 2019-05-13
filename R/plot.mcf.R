#' @export
plot.mcf <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    plot.data = T, title.option = GetSMRDDefault("SMRD.TitleOption"), my.title = NULL, xlab = paste("Time in",
        get.time.units(x)), ylab = "Mean Cumulative Function",
    add = F, lty = 1, plot.seg = T, xlim = c(NA, NA), ylim = c(NA,
        NA), band.type = "p", ci.opt = "solid", col.ci = 4,...)
{
    mcf.table <- get.mcf.table(mcf.out = x, conf.level = conf.level,
        print.table = F)
    if (ci.opt == "solid") {
        lty.ci <- 1
        lwd.ci <- 1
        lwd.mcf <- 3
  } else {
        lty.ci <- 3
        lwd.ci <- 2
        lwd.mcf <- 3
    }
    if (!add) {
        par(mar = c(4.5, 5.1, 3.5, 2.1))
        max.censoring.time <- x$max.censoring.time
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- range(x$tuniq, max.censoring.time,
                0)[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- range(mcf.table[, "mu-hat"], mcf.table[,
                "Lower"], mcf.table[, "Upper"], 0, na.rm = T)[yrna]
        plot.paper(x = xlim, y = ylim, grids = F, xlab = "", ylab = "",
            cex = 1.5)
        title(xlab = xlab, cex.lab = 1.1)
        title(ylab = ylab, cex.lab = 1.1, mgp = c(4, 1, 0))
        if (band.type == "p") {
            ci.title <- paste("\nwith ", floor(conf.level * 100 +
                0.01), "%", "Confidence Intervals", sep = "")
      } else ci.title <- NULL
        if (is.null(my.title))
            my.title <- paste("Mean Cumulative Function for",
                get.data.title(x), ci.title)
        if (title.option == "full")
            title(main = my.title, cex = 0.8)
    }
    max.censoring.time <- x$max.censoring.time
    if (plot.seg) {
        mcf.segments(x$tuniq, mcf.table[, "mu-hat"], max.censoring.time,
            lty = lty, lwd = lwd.mcf)
        if (band.type == "p") {
            mcf.segments(x$tuniq, mcf.table[, "Lower"],
                max.censoring.time, lty = lty.ci, lwd = lwd.ci,
                col = col.ci)
            mcf.segments(x$tuniq, mcf.table[, "Upper"],
                max.censoring.time, lty = lty.ci, lwd = lwd.ci,
                col = col.ci)
        }
  } else {
        mcf.lines(x$tuniq, mcf.table[, "mu-hat"], max.censoring.time,
            lty = lty, lwd = lwd.mcf)
        if (band.type == "p") {
            mcf.lines(x$tuniq, mcf.table[, "Lower"], max.censoring.time,
                lty = lty.ci, lwd = lwd.ci, col = col.ci)
            mcf.lines(x$tuniq, mcf.table[, "Upper"], max.censoring.time,
                lty = lty.ci, lwd = lwd.ci, col = col.ci)
        }
    }
    invisible()
}
