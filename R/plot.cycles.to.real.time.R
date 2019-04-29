plot.cycles.to.real.time <-
function (x, distribution, mu.cycles, sigma.cycles,
    time.range = c(NA, NA), npoints = 100, xlim = c(NA, NA),
    ylim = c(NA, NA),...)
{
    x <- x$x
    probs <- x$probs
    max.use <- max(x)
    xtvna <- is.na(time.range)
    if (is.logdist(distribution)) {
        if (any(xtvna))
            time.range[xtvna] <- (exp(mu.cycles + range(quant(1e-05,
                distribution), quant(0.990000000000001, distribution)) *
                sigma.cycles))[xtvna]
        tvec <- logseq(time.range[1], time.range[2], length = npoints)
    }
    else {
        if (any(xtvna))
            time.range[xtvna] <- (mu.cycles + range(quant(1e-05,
                distribution), quant(0.990000000000001, distribution)) *
                sigma.cycles)[xtvna]
        tvec <- seq(time.range[1], time.range[2], length = npoints)
    }
    tvec <- tvec/max.use
    cdf <- rep(0, length(tvec))
    pdf <- rep(0, length(tvec))
    cdf.matrix <- matrix(0, nrow = length(tvec), ncol = length(x))
    for (i in 1:length(x)) {
        z <- (log(tvec) - (mu.cycles - log(x[i])))/sigma.cycles
        cdf.matrix[, i] <- wqmf.phibf(z, distribution)
        cdf <- cdf + probs[i] * wqmf.phibf(z, distribution)
        pdf <- pdf + (probs[i] * wqmf.phis(z, distribution))/(tvec *
            sigma.cycles)
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(c(50, range(tvec)[2]))[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(c(1e-04, range(cdf.matrix)[2]))[yrna]
    probplot.setup(distribution, xlim = xlim, ylim = ylim,
        grids = F, linear.axes = F, slope.axis = F, xlab = "Weeks of Service")
    for (i in 1:length(x)) {
        lines(log(tvec), quant(cdf.matrix[, i], distribution))
    }
    lines(log(tvec), quant(cdf, distribution), lwd = 3, col = 4)
    return(list(tvec = tvec, pdf = pdf, cdf = cdf))
}
