plot.marginals.sim <-
function (x, focus.quantity, focus.quantity.detail,
    plot.type = "histogram", x.of.interest = NA, xlim = c(NA,
        NA), xlab = NULL, my.title = NULL, cex = 1, ...)
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    marginal.sample <- marginalize.sim(x, focus.quantity = focus.quantity,
        focus.quantity.detail = focus.quantity.detail, x.of.interest = x.of.interest,
        ...)
    unique.in.sample <- unique(marginal.sample)
    number.simulations <- length(marginal.sample)
    if (length(unique.in.sample) <= 1)
        warning(paste("Only one unique value in the simulation results of length",
            paste(number.simulations, ":", sep = ""), unique.in.sample))
    if (is.null(xlab))
        xlab <- attr(marginal.sample, "label")
    switch(plot.type, Histogram = , histogram = {
        hist(marginal.sample, xlab = xlab, col = 4, main = "")
    }, Density = , density = {
        wqm.plot.density(marginal.sample, xlab = xlab, cex = cex)
    }, `Density histogram` = , `density histogram` = {
        hist(marginal.sample, xlab = xlab, col = 4, main = "")
    }, {
        stop(paste("Unknown plot.type", plot.type))
    })
    mean.var.out <- trim.mean.var(marginal.sample)
    sample.sd <- sqrt(mean.var.out$var)
    sample.mean <- mean.var.out$mean
    cat("\nThe mean of the", number.simulations, "simulation estimates of\n",
        attr(marginal.sample, "label"), "is:", format(sample.mean,
            digits = 4), "\n")
    if (map.SMRDDebugLevel() >= 4) {
        cat("CI for the mean is [", format(sample.mean - (1.96 *
            sample.sd)/sqrt(number.simulations), digits = 4),
            format(sample.mean + (1.96 * sample.sd)/sqrt(number.simulations),
                digits = 4), "]\n")
    }
    cat("\nThe standard deviation of the", number.simulations,
        "simulation estimates of\n", attr(marginal.sample, "label"),
        "is:", format(sample.sd, digits = 4), "\n")
    if (map.SMRDDebugLevel() >= 4) {
        cat("CI for the sd is [", format(sample.sd * sqrt((number.simulations -
            1)/qchisq(0.975, number.simulations - 1)), digits = 4),
            format(sample.sd * sqrt((number.simulations - 1)/qchisq(0.025,
                number.simulations - 1)), digits = 4), "]\n")
    }
    conf.level <- GetSMRDDefault("SMRD.ConfLevel")/100
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    parameter.type <- attr(x, "parameter.type")
    if (is.null(parameter.type)) {
        parameter.type <- attr(x, "real")
        parameter.type <- "positive"
    }
    switch(parameter.type, real = {
    }, positive = {
        Rfactor <- exp((zvalue * sqrt(mean.var.out$var))/mean.var.out$mean)
        PrecisionFactorName <- "R precision factor"
    }, probability = {
    })
    cat("\nThe", PrecisionFactorName, "for\n", attr(marginal.sample,
        "label"), "is:", format(Rfactor, digits = 4), "\n\n")
    if (is.null(my.title)) {
        my.title <- attr(x, "title")
    }
    if (is.R())
        title.line = 0.5
    else title.line = 3.2999999
    mtext(text = my.title, line = title.line, side = 3)
    invisible(marginal.sample)
}
