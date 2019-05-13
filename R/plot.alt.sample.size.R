#' @export
plot.alt.sample.size <-
function (x, ALT.plan.values, use.conditions, quantile.of.interest = 0.1,
    xlim = c(NA, NA), ylim = c(NA, NA), conf.levels = c(0.8,
        0.9, 0.95, 0.99), grids = F, my.title = NULL, number.points = 100,...)
{
    if (missing(use.conditions)) {
        if (is.null(ALT.plan.values$use.conditions))
            stop("\n Use conditions have not been specified.\n")
        use.conditions <- ALT.plan.values$use.conditions
    }
    accelvar.units <- ALT.plan.values$accelvar.units
    sigma <- ALT.plan.values$sigma
    distribution <- ALT.plan.values$distribution
    if (is.logdist(distribution)) {
        ylab <- "Confidence Interval Precision Factor R           "
        Rupper<-ifelse(is.na(xlim[2]),3,xlim[2])
        Rrange <- c(1.1, Rupper * 0.9)
        Rvec <- logseq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- logb(Rvec)
    } else {
        Rrange <- logb(c(1.1, 3)) * sigma
        ylab <- paste("Confidence Interval Half-Width D           ",
            get.time.units(x))
        Rvec <- seq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- Rvec
    }
    plan.string <- attr(x, "string.name")
    if (is.null(plan.string))
        plan.string <- deparse(substitute(x))
    plan.values.string <- attr(ALT.plan.values, "string.name")
    if (is.null(plan.values.string))
        plan.values.string <- deparse(substitute(ALT.plan.values))
    if (is.null(my.title)) {
        if (is.logdist(distribution))
            my.title <- paste("Needed sample size giving approximately a 50% chance of having\n a confidence interval factor for the",
                quantile.of.interest, "quantile that is less than R\n",
                plan.string, plan.values.string, "\nuse conditions=",
                paste(use.conditions, accelvar.units, collapse = ","))
        else my.title <- paste("Needed sample size giving approximatley a 50% chance of having\n a confidence interval half-width for the",
            quantile.of.interest, "quantile that is less than D\n",
            plan.string, plan.values.string, "\nuse conditions=",
            paste(use.conditions, accelvar.units, collapse = ","))
    }
    evaluate.results <- evaluate.alt.test.plan(x = x,
        use.conditions = use.conditions, ALT.plan.values, quantile.of.interest = quantile.of.interest)
    sample.size <- attr(evaluate.results, "sample.size")
    if (is.logdist(distribution)) {
        variance.factor <- sample.size * ((evaluate.results[3])/evaluate.results[2])^2
    } else {
        variance.factor <- sample.size * (evaluate.results[3])^2
    }
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

    mtext(text = my.title, side = 3, line = 0.5, cex = 1.2)
}
