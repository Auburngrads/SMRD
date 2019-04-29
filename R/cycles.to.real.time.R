cycles.to.real.time <-
function (use.rate.distribution, distribution.cycles, mu.cycles,
    sigma.cycles, time.range = c(NA, NA), npoints = 100)
{
    x <- use.rate.distribution$x
    probs <- use.rate.distribution$probs
    max.use <- max(x)
    xtvna <- is.na(time.range)
    if (is.logdist(distribution.cycles)) {
        if (any(xtvna))
            time.range[xtvna] <- (exp(mu.cycles + range(quant(1e-05,
                distribution.cycles), quant(0.9999, distribution.cycles)) *
                sigma.cycles))[xtvna]
        tvec <- logseq(time.range[1], time.range[2], length = npoints)
    }
    else {
        if (any(xtvna))
            time.range[xtvna] <- (mu.cycles + range(quant(1e-05,
                distribution.cycles), quant(0.9999, distribution.cycles)) *
                sigma.cycles)[xtvna]
        tvec <- seq(time.range[1], time.range[2], length = npoints)
    }
    tvec <- tvec/max.use
    cdf <- rep(0, length(tvec))
    pdf <- rep(0, length(tvec))
    for (i in 1:length(x)) {
        z <- (log(tvec) - (mu.cycles - log(x[i])))/sigma.cycles
        cdf <- cdf + probs[i] * wqmf.phibf(z, distribution.cycles)
        pdf <- pdf + (probs[i] * wqmf.phis(z, distribution.cycles))/(tvec *
            sigma.cycles)
    }
    return(list(tvec = tvec, pdf = pdf, cdf = cdf))
}
