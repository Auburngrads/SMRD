ADDT.plot.FracFail.v.Time <-
function (results.object, x.of.interest = NULL, FailLevel = NULL,
    plan.values.string = NULL, plan.string = NULL, quantile.range = c(0.01,
        0.99), ylim = c(NA, NA), xlim = c(NA, NA), xlab = NULL,
    ylab = NULL, my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F,
    numplotsim = 50, nxpoints = 50, cex = 1, plot.distribution = "lognormal",
    how.time.range = "modified test plan")
{
    use.condition <- x.of.interest
    number.sim <- nrow(results.object)
    if (is.null(use.condition))
        use.condition <- attr(results.object, "use.condition")
    if (is.null(use.condition))
        stop("Use condition not specified")
    if (is.character(use.condition))
        use.condition <- string.to.frame(use.condition)
    if (is.null(FailLevel))
        FailLevel <- attr(results.object, "FailLevel")
    ADDT.plan.values <- attr(results.object, "plan.values")
    ADDT.test.plan <- attr(results.object, "plan")
    par(err = -1)
    if (is.null(plan.string))
        plan.string <- attr(results.object, "plan.string")
    if (is.null(plan.values.string))
        plan.values.string <- attr(results.object, "plan.values.string")
    FailLevelDef <- paste(FailLevel, get.response.units(ADDT.plan.values))
    if (is.null(xlab))
        xlab <- get.time.units(ADDT.plan.values)
    if (is.null(ylab))
        ylab <- GetSMRDDefault("SMRD.LabelOnYaxis")
    distribution <- ADDT.plan.values$distribution
    transformation.x <- ADDT.plan.values$transformation.x
    transformation.time <- ADDT.plan.values$transformation.time
    transformation.response <- ADDT.plan.values$transformation.response
    model.string <- paste("Resp:", transformation.response, ",Time:",
        transformation.time, ",x:", paste(ADDT.plan.values$transformation.x,
            collapse = ","), ", Dist:", distribution, sep = "")
    if (is.null(my.title))
        my.title <- paste("Accelerated destructive degradation test simulation based on\n",
            plan.string, plan.values.string, "\nFraction failing versus",
            xlab, "for ", FailLevelDef, "at", paste(use.condition,
                ADDT.plan.values$accelvar.units, collapse = ","),
            "\n", model.string)
    transformation.x <- fix.inverse.relationship(transformation.x)
    slope.name <- attr(transformation.x, "slope.name")
    y.axis <- "log"
    numplotsim <- min(number.sim, numplotsim)
    the.model <- list(distribution = distribution, transformation.x = transformation.x,
        transformation.time = transformation.time, transformation.response = transformation.response)
    Dummy.Dest.Degrad.out <- list(dummy = T, origparam = ADDT.plan.values$theta.vec,
        origparamvcv = diag(length(ADDT.plan.values$theta.vec)),
        model = the.model)
    oldClass(Dummy.Dest.Degrad.out) <- "gmle.out"
    plan.time.range <- range(times(ADDT.test.plan))
    full.dist.time.range <- range(fx.ADDT.life.quantile(theta.hat = Dummy.Dest.Degrad.out$origparam,
        p = quantile.range[1], distribution = Dummy.Dest.Degrad.out$model$distribution,
        FailLevel = FailLevel, xuse = use.condition, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
        transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
        transformation.time = Dummy.Dest.Degrad.out$model$transformation.time),
        fx.ADDT.life.quantile(theta.hat = Dummy.Dest.Degrad.out$origparam,
            p = quantile.range[2], distribution = Dummy.Dest.Degrad.out$model$distribution,
            FailLevel = FailLevel, xuse = use.condition, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
            transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
            transformation.time = Dummy.Dest.Degrad.out$model$transformation.time))
    switch(how.time.range, `test plan` = {
        derived.time.range <- plan.time.range
    }, `entire distribution` = {
        derived.time.range <- full.dist.time.range
    }, `modified test plan` = {
        if (plan.time.range[1] < full.dist.time.range[1] && full.dist.time.range[1] >
            plan.time.range[2]) derived.time.range <- c(plan.time.range[1],
            full.dist.time.range[1]) else derived.time.range <- full.dist.time.range
    })
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- derived.time.range[xrna]
    if (xlim[1] <= 0)
        xlim[1] <- 0.001
    time.vec <- logseq(xlim[1], xlim[2], length = nxpoints)
    frac.fail.true <- fx.ADDT.life.failure.probability(theta.hat = Dummy.Dest.Degrad.out$origparam,
        time.vec = time.vec, distribution = Dummy.Dest.Degrad.out$model$distribution,
        FailLevel = FailLevel, xuse = use.condition, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
        transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
        transformation.time = Dummy.Dest.Degrad.out$model$transformation.time)
    uber.results.object <- matrix(results.object[1:nrow(results.object),
        1:ncol(results.object), drop = FALSE], ncol = ncol(results.object),
        nrow = nrow(results.object), byrow = F)
    frac.fail.mat <- (apply(uber.results.object[, 1:length(ADDT.plan.values$theta.vec),
        drop = F], 1, fx.ADDT.life.failure.probability, time.vec = time.vec,
        distribution = Dummy.Dest.Degrad.out$model$distribution,
        FailLevel = FailLevel, xuse = use.condition, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
        transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
        transformation.time = Dummy.Dest.Degrad.out$model$transformation.time))
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(frac.fail.mat)[yrna]

    probplot.setup(plot.distribution, ylim = ylim, xlim = xlim,
        my.title = my.title, title.option = title.option, cex = cex,
        xlab = xlab, ylab = ylab, grids = grids, title.line.adj = -1.5)
    take.out <- c(1, 2, length(time.vec) - 1, length(time.vec))
    lines(logb(time.vec), pp.quant(frac.fail.true, plot.distribution),
        col = 1, lwd = 4, lty = 1)
    matlines(logb(time.vec[-take.out]), pp.quant(frac.fail.mat[-take.out,
        1:numplotsim], plot.distribution), col = 1, lty = 2)
    invisible()
}
