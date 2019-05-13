#' @export
plot.ADDT.sample.size <-
function (x, ADDT.plan.values, use.condition, FailLevel,
    quantile.of.interest = 0.1, xlim = c(NA, NA), ylim = c(NA,
        NA), conf.levels = c(0.8, 0.9, 0.95, 0.99), grids = F,
    my.title = NULL, number.points = 100,...)
{
    plan.string <- attr(x, "string.name")
    if (is.null(plan.string))
        plan.string <- deparse(substitute(x))
    plan.values.string <- attr(ADDT.plan.values, "string.name")
    if (is.null(plan.values.string))
        plan.values.string <- deparse(substitute(ADDT.plan.values))
    if (missing(use.condition)) {
        use.condition <- ADDT.plan.values$use.condition
        if (is.null(use.condition))
            stop("Use conditions must be specified")
    }
    use.condition <- string.to.frame(use.condition)
    sigma <- ADDT.plan.values$sigma
    if (missing(FailLevel)) {
        FailLevel <- ADDT.plan.values$FailLevel
        if (is.null(FailLevel))
            stop("FailLevel must be specified")
    }
    x <- hframe.to.vframe(x)
    sample.size <- sum(allocation(x))
    if (map.SMRDDebugLevel() >= 4)
        cat("Original sample size=", sample.size, "\n")
    distribution <- ADDT.plan.values$distribution
    transformation.time <- ADDT.plan.values$transformation.time
    transformation.x <- fix.inverse.relationship(ADDT.plan.values$transformation.x)
    transformation.response <- ADDT.plan.values$transformation.response
    theta.hat <- ADDT.plan.values$theta.vec
    ADDT.model <- .pseudo.model(ADDT.plan.values, ADDT.test.plan = x)
    gamma.hat <- .f.ADDT.stableparam(theta.hat, model = ADDT.model)
    theta2.hat <- .f.ADDT.origparam(gamma.hat, model = ADDT.model)
    vcv.gamma <- ADDT.vcv(ADDT.plan.values, ADDT.test.plan = x)$the.tran.vcv
    if (T || is.logdist(distribution)) {
        ylab <- "Confidence Interval Precision Factor R           "
        if (is.na(xlim[2]))
            Rupper <- 3
        else Rupper <- xlim[2]
        Rrange <- c(1.1, Rupper * 0.9)
        Rvec <- logseq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- logb(Rvec)
}    else {
        Rrange <- logb(c(1.1, 3)) * sigma
        ylab <- paste("Confidence Interval Half-Width D           ",
            get.time.units(x))
        Rvec <- seq(Rrange[1], Rrange[2], length = number.points)
        xRvec <- Rvec
    }
    if (is.null(my.title)) {
        if (T || is.logdist(distribution))
            my.title <- paste("Needed sample size giving approximately a 50% chance of having\n a confidence interval factor for the",
                quantile.of.interest, "quantile that is less than R\n",
                "use condition =", paste(use.condition, ADDT.plan.values$accelvar.units,
                  collapse = ", "), "and a failure definition =",
                FailLevel, "\n", plan.string, plan.values.string)
}    else  {my.title <- paste("Needed sample size giving approximatley a 50% chance of having\n a confidence interval half-width for the",
            quantile.of.interest, "quantile that is less than D\n",
            plan.string, plan.values.string, "use condition =",
            paste(use.condition, ADDT.plan.values$accelvar.units,
                collapse = ", "))
    }
    answer <- f.gendeltamethod(vcv.gamma, gamma.hat, fx.ADDT.life.quantile.gamma,
        p = quantile.of.interest, distribution = distribution,
        FailLevel = FailLevel, xuse = use.condition, transformation.response = transformation.response,
        transformation.x = transformation.x, transformation.time = transformation.time,
        model = ADDT.model)
    if (answer$vec <= 0) {
        stop(paste("The failure-time", quantile.of.interest,
            "quantile for the given planning values, and failure level is 0.\nTry a lower failure level or a larger quantile."))
    }
    variance.factor <- sample.size * answer$se^2
    the.quantile <- answer$vec
    ymatrix <- matrix(0, ncol = length(conf.levels), nrow = number.points)
    for (i in 1:length(conf.levels)) {
        zvalue <- quant(1 - (1 - conf.levels[i])/2, "normal")
        ymatrix[, i] <- ((zvalue^2) * variance.factor)/((the.quantile *
            xRvec)^2)
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
    logymat <- logb(ymatrix)
    matlines(Rvec, logymat, col = 1, lty = 1, lwd = 2)
    abline(v = 1, lwd = 2, lty = 4)
    for (i in 1:length(conf.levels)) {
        the.yloc <- logymat[number.points, i]
        text(x.loc(0.95), the.yloc, percent.conf.level(conf.levels[i]))
    }

    mtext(text = my.title, side = 3, line = 0.5,
        cex = 1.2)
}
