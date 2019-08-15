wqm.plot.density.use.rate.model <-
function (use.rate.model.out, npoints = 200, xlab = NULL, my.title = NULL,
    allow.recip = F, ...)
{
    if (is.null(xlab))
        xlab <- "Cycles per Unit Time"
    if (is.null(my.title))
        my.title <- "Use Rate Density"
    lab.time.units <- use.rate.model.out$lab.time.units
    field.time.units <- use.rate.model.out$field.time.units
    mu.use.rate <- use.rate.model.out$mu.use.rate
    sigma.use.rate <- use.rate.model.out$sigma.use.rate
    distribution.use.rate <- use.rate.model.out$distribution.use.rate
    if (allow.recip && mu.use.rate < 0) {
        switch <- T
        mu.use.rate <- -mu.use.rate
        xlab <- paste(field.time.units, "per", lab.time.units)
    }
    else {
        switch <- F
        if (lab.time.units == strip.s(lab.time.units))
            lab.time.units <- paste(lab.time.units, "s", sep = "")
        xlab <- paste(lab.time.units, "per", strip.s(field.time.units))
    }
    time.range <- (exp(mu.use.rate + range(quant(0.001, distribution.use.rate),
        quant(0.999, distribution.use.rate)) * sigma.use.rate))
    tvec <- logseq(time.range[1], time.range[2], length = npoints)
    the.density <- dlnorm(tvec, mu.use.rate, sigma.use.rate)
    plot.paper(x = range(tvec), y = range(the.density *
        tvec), x.axis = "log", ylab = "", xlab = xlab, grids = F,
        y.axis = "blank", ...)
    lines(logb(tvec), the.density * tvec)
    title(my.title)
}
