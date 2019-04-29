bisa.ln.cf <-
function (bisa.shape.vec = c(0.1, 0.5, 1, 1.5, 3), number.points = 100,
    meanlog = 0, sdlog = 1, pquantile = 0.5, ylim = c(0.01,
        0.99), time.range = c(0.09, 19))
{
    lognor.quantile <- qlnorm(pquantile, meanlog = meanlog, sdlog = sdlog)
    number.shapes <- length(bisa.shape.vec)
    ltyvec <- seq(2, by = 1, length = number.shapes)
    cdfmat <- matrix(NA, nrow = number.points, ncol = number.shapes)
    if (is.null(time.range))
        time.range <- qlnorm(ylim, meanlog = meanlog, sdlog = sdlog)
    time.vec <- logseq(time.range[1], time.range[2], length = number.points)
    for (i in 1:number.shapes) {
        bisa.shape.now <- bisa.shape.vec[i]
        bisa.scale.now <- lognor.quantile/qbisa(pquantile, shape = bisa.shape.now)
        cdfmat[, i] <- pbisa(time.vec, scale = bisa.scale.now,
            shape = bisa.shape.now)
        xbq <- qbisa(0.5, scale = bisa.scale.now, shape = bisa.shape.now)
    }
    log.of.data <- probplot.setup("lognormal", range(time.vec),
        ylim, sub.title = "", cex = 1.5,
        cexlab = 1.5, grids = 0, linear.axes = F, title.option = "blank",
        slope.axis = F, ylab = "Proportion Failing")
    for (i in 1:number.shapes) {
        lines(pp.data(time.vec, log.of.data), quant(cdfmat[,
            i], "lognormal"), lty = ltyvec[i], lwd = 2)
    }
    title(xlab = "Time", cex = 1.2)

    text(0.462283, 2.56613, "beta=.1", cex = 1.3)
    text(1.37341, 2.45092, 0.5, cex = 1.3)
    text(2.31828, 2.53471, 1, cex = 1.3)
    text(2.66417, 2.01101, 1.5, cex = 1.3)
    text(2.69791, 0.837921, 3, cex = 1.3)
}
