plot.warranty.gmle.out <-
function (x, add = F, xlim = c(NA, NA), ylim = c(NA,
    NA), sigma.factor = 2.5, miles.limit = NULL, time.limit = NULL,
    plot.failures = T, cex = 1,...)
{
    par(mar = c(6, 8, 4, 6) + 0.1)
    mu.time <- x$origparam[1]
    sigma.time <- x$origparam[2]
    mu.dist <- x$origparam[3]
    sigma.dist <- x$origparam[4]
    rho <- x$origparam[5]
    the.bvn.range <- range.bvn.gmle.out(x, sigma.factor = sigma.factor)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- the.bvn.range$xlim[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- the.bvn.range$ylim[yrna]
    plot.paper(xlim, ylim, x.axis = "log", y.axis = "log",
        grids = F, cex.tic.lab = cex)
    if (plot.failures)
        points.default(logb(xmat(x$data.ld)[,
            1]), logb(Response(x$data.ld)), pch = ".")
    mtext(text = "Months in Service", side = 1, line = 3, cex = cex *
        1.5)
    mtext(text = "Thousands of Miles", side = 2, line = 4, cex = cex *
        1.5)
    plot.bvn(mu.time, mu.dist, sigma.time, sigma.dist, rho, xlim = logb(xlim),
        ylim = logb(ylim), relative = T, add = T, cex = cex *
            1.2, v = c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9))
    if (!is.null(miles.limit) && !is.null(time.limit)) {
        abline(v = logb(time.limit))
        abline(h = logb(miles.limit))
        failure.probability <- bvnsw(mu.time, mu.dist, sigma.time,
            sigma.dist, rho, logb(time.limit), logb(miles.limit))
        text(x.loc(0.95), y.loc(0.05), paste("Failure Probability =",
            format(failure.probability, digits = 4)), adj = 1)
    }
}
