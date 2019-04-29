plot.function.task.marginals <-
function (x, post.or.prior, marginal.on, marginal.on.detail,
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, interval.type = "two-sided",
    xlab = NULL, xlim = c(NA, NA), ylim = c(NA, NA), print.ci = T,
    x.of.interest = NULL, my.title = NULL, digits = GetSMRDDefault("SMRD.DigitsPrinted"),...)
{
    func.call <- match.call()
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    conditional.on.data <- single.ifelse(post.or.prior == "post",
        " | DATA", "")
    marginal.sample <- marginalize.task(x, post.or.prior,
        marginal.on = marginal.on, marginal.on.detail = marginal.on.detail,
        x.of.interest = x.of.interest)
    distribution <- generic.distribution(x$distribution)
    marginal.trim.range <- range(trim.vector(marginal.sample,
        trim = 0.005))
    x.width <- (4 * (marginal.trim.range[2] - marginal.trim.range[1]))/(logb(length(marginal.sample),
        base = 2) + 1)
    density.out <- density(marginal.sample, width = x.width,
        from = marginal.trim.range[1], to = marginal.trim.range[2])
    switch(marginal.on, Quantile = , quantile = {
        p.for.quantile <- marginal.on.detail
        if (is.null(xlab)) xlab <- paste("t_", p.for.quantile,
            sep = "")
        ylab <- paste("f(t_", p.for.quantile, conditional.on.data,
            ")", sep = "")
    }, `failure probability` = , `Failure probability` = {
        t.for.failure.prob <- marginal.on.detail
        xlab <- paste("F(", t.for.failure.prob, ")", sep = "")
        ylab <- paste("f(F(", t.for.failure.prob, conditional.on.data,
            ")", sep = "")
    }, Parameter = , parameter = {
        switch(marginal.on.detail, spread = , sigma = , beta = {
            if (distribution == "weibull") parameter.name <- "beta" else parameter.name <- "sigma"
        }, mu = , eta = {
            if (distribution == "weibull") parameter.name <- "eta" else parameter.name <- "mu"
        })
        xlab <- parameter.name
        ylab <- paste("f(", parameter.name, conditional.on.data,
            ")", sep = "")
    })
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(density.out$x)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(density.out$y)[yrna]
    plot.paper(xlim, ylim, xlab = xlab, ylab = ylab, y.axis = "blank",
        grids = F)
    lines(density.out, lwd = 2)
    abline(h = 0)
    length.post <- length(marginal.sample)
    marginal.sample <- sort(marginal.sample)
    mean.of.marginal <- mean(marginal.sample)
    if (print.ci)
        cat("\nThe mean of the posterior distribution of", xlab,
            "is:", format(mean.of.marginal, digits = digits),
            "\n")
    switch(interval.type, `two-sided` = {
        lowerq <- (1 - conf.level)/2
        point.to.pick <- ceiling(lowerq * length.post)
        lower.bound <- marginal.sample[point.to.pick]
        abline(v = lower.bound, lty = 2)
        point.to.pick <- floor((1 - lowerq) * length.post + 0.5)
        upper.bound <- marginal.sample[point.to.pick]
        abline(v = upper.bound, lty = 2)
        if (print.ci) cat("The", percent.conf.level(conf.level),
            "Bayesian credibility interval is:", paste("[", format(lower.bound,
                digits = digits), ",   ", format(upper.bound,
                digits = digits), "]", sep = ""), "\n")
    }, lower = {
        point.to.pick <- ceiling((1 - conf.level) * length.post)
        lower.bound <- marginal.sample[point.to.pick]
        abline(v = lower.bound, lty = 2)
        print(paste("Lower confidence bound:", paste(format(c(lower.bound),
            digits = digits), collapse = ",")))
    }, upper = {
        point.to.pick <- floor((conf.level) * length.post + 0.5)
        upper.bound <- marginal.sample[point.to.pick]
        abline(v = upper.bound, lty = 2)
        print(paste("Upper confidence bound:", paste(format(c(upper.bound),
            digits = digits), collapse = ",")))
    }, none = {
    }, {
        stop(paste("illegal interval type", interval.type))
    })
    if (is.null(my.title)) {
        sample.name <- single.ifelse(post.or.prior == "post",
            "Posterior", "Prior")
        my.title <- paste(x$distribution, "Model",
            sample.name, "Distribution for", get.data.title(x$data.ld))
    }
    title(my.title, cex = 0.8)
    invisible()
}
