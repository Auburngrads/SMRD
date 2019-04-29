plot.asym.sample.size <-
function (x, censor.time, fraction.failing, quantile.of.interest,
    xlim = c(NA, NA), ylim = c(NA, NA), conf.levels = c(0.8,
        0.9, 0.95, 0.99), grids = F, my.title = NULL, number.points = 100,...)
{
    time.units <- x$time.units
    mu <- x$mu
    sigma <- x$sigma
    distribution <- generic.distribution(x$distribution)
    if (is.logdist(distribution)) {
        if (!missing(censor.time))
            log.censor.time <- logb(censor.time)
        ylab <- "Confidence Interval Precision Factor R      "
        Rrange <- c(1.1, 3)
        Rvec <- logseq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- logb(Rvec)
}   else {
        if (!missing(censor.time))
            log.censor.time <- censor.time
        Rrange <- logb(c(1.1, 3)) * sigma
        ylab <- paste("Confidence Interval Half-Width D", time.units)
        Rvec <- seq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- Rvec
    }
    if (missing(fraction.failing)) {
        if (missing(censor.time))
            stop("Need to specify either censor.time or fraction.failing")
        censoring.type <- "Type I"
        std.log.censor.time <- (log.censor.time - mu)/sigma
        std.quantile.of.interest <- quant(quantile.of.interest,
            distribution)
        fraction.failing <- wqmf.phibf(std.log.censor.time, distribution)
}   else {
        if (!missing(censor.time))
            stop("Cannot specify both censor.time or fraction.failing")
        censoring.type <- "Type II"
        if (fraction.failing < 0 || fraction.failing > 1)
            stop(paste("fraction.failing", fraction.failing,
                " must be greater than 0 and less than or equal to 1"))
}
        if (fraction.failing < 1) {
            std.log.censor.time <- quant(fraction.failing, distribution)
            log.censor.time <- mu + std.log.censor.time * sigma
    }   else {
            std.log.censor.time <- quant(0.99999999, distribution)
            log.censor.time <- mu + std.log.censor.time * sigma
        }
        std.quantile.of.interest <- quant(quantile.of.interest,
            distribution)
        if (is.logdist(distribution)) {
            censor.time <- exp(log.censor.time)
}       else {censor.time <- log.censor.time
}
    if (numdist(distribution) == 2) {
        plan.string <- paste(distribution, "Distribution with eta=",
            format(exp(mu)), " and beta=", format(1/sigma))
}   else {
        plan.string <- paste(distribution, "Distribution with mu=",
            format(mu), " and sigma=", format(sigma))
}
    if (censoring.type == "Type I") {
        censor.string <- paste("\nTest censored at", format(censor.time,
            digits = 4), time.units, "with", format(100 * fraction.failing,
            digits = 3), "expected percent failing")
}   else {
        censor.string <- paste("\nTest terminated after", format(100 *
            fraction.failing, digits = 3), "percent failing")
}
    if (is.null(my.title)) {
        if (is.logdist(distribution))
            my.title <- paste("Needed sample size giving approximately a 50% chance of having\n a confidence interval factor for the",
                quantile.of.interest, "quantile that is less than R\n",
                plan.string, censor.string)
}   else {my.title <- paste("Needed sample size giving approximatley a 50% chance of having\n a confidence interval half-width for the",
            quantile.of.interest, "quantile that is less than D\n",
            plan.string, censor.string)
    }
    var.out <- ftavarvec(x$distribution, std.log.censor.time)
    variance.factor <- (x$sigma^2) * (var.out$v11 +
        2 * std.quantile.of.interest * var.out$v12 + (std.quantile.of.interest)^2 *
        var.out$v22)
    ymatrix <- matrix(0, ncol = length(conf.levels), nrow = number.points)
    for (i in 1:length(conf.levels)) {
        zvalue <- quant(1 - (1 - conf.levels[i])/2, "normal")
        ymatrix[, i] <- ((zvalue^2) * variance.factor)/(xRvec^2)
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(Rvec)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(ymatrix)[yrna]
    plot.paper(xlim, ylim, x.axis = "linear", y.axis = "log",
        grids = grids, cex = 1, cex.labs = 1.2, ylab = "Sample Size",
        xlab = ylab)
    abline(v = 1, lwd = 2, lty = 4)
    logymat <- logb(ymatrix)
    matlines(Rvec, logymat, col = 1, lty = 1, lwd = 2)
    for (i in 1:length(conf.levels)) {
        the.yloc <- logymat[number.points, i]
        text(x.loc(0.95), the.yloc, percent.conf.level(conf.levels[i]))
    }
    mtext(text = my.title, side = 3, line = 4, cex = 1.2)
}
