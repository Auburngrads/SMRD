#' @export
plot.function.marginals <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    interval.type = "two-sided", p.for.quantile = NULL, x.of.interest = NULL,
    xlab = NULL, t.for.failure.prob = NULL, scale.factor = 1,
    xlim = c(NA, NA), ylim = c(NA, NA), print.ci = T,...)
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    marginal.sample <- marginalize(x, p.for.quantile = p.for.quantile,
        x.of.interest = x.of.interest, t.for.failure.prob = t.for.failure.prob)$post/scale.factor
    marginal.trim.range <- range(trim.vector(marginal.sample,
        trim = 0.005))
    x.width <- (2 * (marginal.trim.range[2] - marginal.trim.range[1]))/(logb(length(marginal.sample),
        base = 2) + 1)
    if (!is.null(p.for.quantile)) {
        if (is.null(xlab))
            xlab <- paste("t_", p.for.quantile, sep = "")
        the.range <- ylab <- paste("f(t_", p.for.quantile, " | DATA)",
            sep = "")
        density.out <- density(marginal.sample, window = "r",
            n = 100, cut = 0, width = x.width)
}   else {
        if (!is.null(t.for.failure.prob)) {
            xlab <- paste("F(", t.for.failure.prob, " | DATA)",
                sep = "")
            ylab <- paste("f(F(", t.for.failure.prob, ") | DATA)",
                sep = "")
            density.out <- density(marginal.sample, window = "r",
                n = 100, cut = 0, width = x.width)
}    else {
            stop("Must specify either p.for.quantile or t.for.failure.prob")
        }
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(density.out$x)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(density.out$y)[yrna]
    plot(xlim, ylim * 1.04, type = "n", xlab = xlab, ylab = "",
        yaxt = "n", yaxs = "i")
    lines(density.out, lwd = 2)
    abline(h = 0)
    length.post <- length(marginal.sample)
    marginal.sample <- sort(marginal.sample)
    switch(interval.type, `two-sided` = {
        lowerq <- (1 - conf.level)/2
        point.to.pick <- floor(lowerq * length.post + 0.5)
        lower.bound <- marginal.sample[point.to.pick]
        abline(v = lower.bound, lty = 2)
        point.to.pick <- floor((1 - lowerq) * length.post + 0.5)
        upper.bound <- marginal.sample[point.to.pick]
        abline(v = upper.bound, lty = 2)
        if (print.ci) cat("The", percent.conf.level(conf.level),
            "Bayesian confidence interval is:", format(c(lower.bound,
                upper.bound), digits = 4), "\n")
    }, lower = {
        point.to.pick <- floor((1 - conf.level) * length.post +
            0.5)
        lower.bound <- marginal.sample[point.to.pick]
        abline(v = lower.bound, lty = 2)
        print(paste("Lower confidence bound:", paste(format(c(lower.bound),
            digits = 4), collapse = ",")))
    }, upper = {
        point.to.pick <- floor((conf.level) * length.post + 0.5)
        upper.bound <- marginal.sample[point.to.pick]
        abline(v = upper.bound, lty = 2)
        print(paste("Upper confidence bound:", paste(format(c(upper.bound),
            digits = 4), collapse = ",")))
    }, none = {
    }, stop("illegal interval type"))
    invisible()
}
