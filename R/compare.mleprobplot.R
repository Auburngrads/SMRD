#' Title
#'
#' @param data.ld 
#' @param main.distribution 
#' @param compare.distribution 
#' @param gamthr 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param my.title 
#' @param sub.title 
#' @param grids 
#' @param linear.axes 
#' @param slope.axis 
#' @param band.type 
#' @param xlab 
#' @param conf.level 
#' @param cex 
#' @param title.option 
#' @param trunc.correct 
#' @param do.legend 
#' @param plot.censored.ticks 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1, 
#'                                 censor.column = 3,
#'                                 time.units = "Kilometers")
#' 
#' mleprobplot(ShockAbsorber.ld, distribution = "Weibull")
#' mleprobplot(ShockAbsorber.ld, distribution = "loglogistic")
#' mleprobplot(ShockAbsorber.ld, distribution = "lognormal")
#' mleprobplot(ShockAbsorber.ld, distribution = "frechet")
#' 
#' 
#' #compare lognormal and Weibull distributions
#' 
#' compare.mleprobplot(ShockAbsorber.ld, 
#'                     main.distribution = "Lognormal",
#'                     compare.distribution ="Weibull")
#'                     
#' compare.mleprobplot(ShockAbsorber.ld, main.distribution= "Lognormal",compare.distribution=c("Weibull","Loglogistic"))
#' 
#' compare.mleprobplot(ShockAbsorber.ld, 
#'                     main.distribution = "Lognormal",
#'                     xlim = c(100,100000),
#'                     ylim = c(.005,.9),
#'                     compare.distribution = c("Weibull","Loglogistic"))
#' 
#' compare.mleprobplot(ShockAbsorber.ld, 
#'                     main.distribution = "Lognormal",
#'                     xlim = c(100,50001),
#'                     ylim = c(.005,.9),
#'                     compare.distribution = c("Weibull","Loglogistic","Exponential"),
#'                     band.type = "chull")
#' 
#' compare.mleprobplot(ShockAbsorber.ld, 
#'                     main.distribution = "Lognormal",
#'                     xlim = c(100,100000),
#'                     ylim = c(.005,.9),
#'                     compare.distribution = c("Weibull","Exponential"),
#'                     band.type = "chull")
#' 
#' }
compare.mleprobplot <-
function (data.ld, main.distribution, compare.distribution, gamthr = 0,
    xlim = c(NA, NA), ylim = c(NA, NA), time.range = c(NA,
        NA), my.title = NULL, sub.title = NULL, grids = F, linear.axes = F,
    slope.axis = F, band.type = "none", xlab = get.time.units(data.ld),
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, cex = 1.2,
    title.option = GetSMRDDefault("SMRD.TitleOption"), trunc.correct = T, do.legend = T,
    plot.censored.ticks = F)
{
    if (is.null(xlab)) {
        xlab <- get.time.units(data.ld)
        if (!is.null(gamthr) && gamthr != 0)
            xlab <- paste(xlab, "-", gamthr)
    }
    if (is.null(my.title))
        my.title <- get.data.title(data.ld)
    cdfest.out <- cdfest(data.ld, gamthr = gamthr)
    cdpoints.out <- cdpoints(cdfest.out)
    parametric.list <- list()
    if (is.R())
        title.line.adj = -2
    else title.line.adj = 0
    if (!is.null(cdfest.out$left.trun.cond) || !is.null(cdfest.out$right.trun.cond)) {
        trunc.correct <- (!is.null(cdfest.out$left.trun.cond) ||
            !is.null(cdfest.out$right.trun.cond)) && trunc.correct
        mlest.out <- mlest(data.ld, main.distribution, gamthr = gamthr)
        if (trunc.correct)
            cdpoints.out <- truncadj(cdpoints.out, mlest.out)
    }
    tvrna <- is.na(time.range)
    if (any(tvrna))
        time.range[tvrna] <- range(cdpoints.out$yplot)[tvrna]
    time.vec <- seq(time.range[1], time.range[2], length = 500)
    main.mlest.out <- mlest(data.ld, distribution = main.distribution)
    parametric.list[[generic.distribution(main.distribution)]] <- main.mlest.out
    main.dist.failure.probabilities <- failure.probabilities.mlest(main.mlest.out,
        time.vec = time.vec, conf.level = conf.level)
    compare.dist.failure.probabilities <- list()
    for (i in 1:length(compare.distribution)) {
        compare.mlest.out <- mlest(data.ld, distribution = generic.distribution(compare.distribution[i]))
        parametric.list[[generic.distribution(compare.distribution[i])]] <- compare.mlest.out
        compare.dist.failure.probabilities[[i]] <- failure.probabilities.mlest(compare.mlest.out,
            time.vec = time.vec, conf.level = conf.level)
    }
    if (length(compare.distribution) == 1) {
        other.fit <- paste(compare.distribution, "Distribution ML Fit")
}   else {
        other.fit <- "Other Distribution ML Fits"
    }
    global.y.max <- max(main.dist.failure.probabilities[, c("Fhat",
        "95% Lower", "95% Upper")])
    global.y.min <- min(main.dist.failure.probabilities[, c("Fhat",
        "95% Lower", "95% Upper")])
    switch(casefold(band.type), pointwise = {
        lower.ci <- mono.lower(main.dist.failure.probabilities[,
            "95% Lower"])
        upper.ci <- mono.upper(main.dist.failure.probabilities[,
            "95% Upper"])
        global.y.max <- max(global.y.min, lower.ci, upper.ci)
        global.y.min <- min(global.y.max, lower.ci, upper.ci)
        ci.description <- paste(percent.conf.level(conf.level),
            "Pointwise Confidence Intervals")
    }, chull = {
        lower.ci <- mono.lower(main.dist.failure.probabilities[,
            "95% Lower"])
        upper.ci <- mono.upper(main.dist.failure.probabilities[,
            "95% Upper"])
        for (i in 1:length(compare.distribution)) {
            lower.ci <- pmin(lower.ci, mono.lower(compare.dist.failure.probabilities[[i]][,
                "95% Lower"]))
            upper.ci <- pmax(upper.ci, mono.upper(compare.dist.failure.probabilities[[i]][,
                "95% Upper"]))
            global.y.min <- max(global.y.min, lower.ci, upper.ci)
            global.y.max <- max(global.y.max, lower.ci, upper.ci)
        }
        ci.description <- paste("\nConvex Hull of All", percent.conf.level(conf.level),
            "Pointwise Confidence Intervals")
    }, none = {
    }, stop(paste("Band type =", band.type, "not recognized")))
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(cdpoints.out$pplot, global.y.min,
            global.y.max)[yrna]
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(time.range, cdpoints.out$yplot)[xrna]
    log.of.data <- probplot.setup(main.distribution, xlim,
        ylim, my.title = my.title, sub.title = sub.title,
        cex = cex, grids = grids, linear.axes = linear.axes,
        slope.axis = slope.axis, title.option = title.option,
        xlab = xlab, title.line.adj = title.line.adj)
    lines(pp.data(main.dist.failure.probabilities[, 1], log.of.data),
        quant(main.dist.failure.probabilities[, "Fhat"], main.distribution),
        col = map.colors(1), lty = 1, lwd = 3)
    points.default(pp.data(cdpoints.out$yplot, log.of.data),
        quant(cdpoints.out$pplot, main.distribution), cex = 1.2,
        pch = 16)
    for (i in 1:length(compare.distribution)) {
        lines(pp.data(compare.dist.failure.probabilities[[i]][,
            1], log.of.data), quant(compare.dist.failure.probabilities[[i]][,
            "Fhat"], main.distribution), col = map.colors(2),
            lty = 4, lwd = 3)
    }
    if (band.type != "none") {
        lines(pp.data(main.dist.failure.probabilities[, 1], log.of.data),
            quant(lower.ci, main.distribution), col = map.colors(4),
            lty = 3, lwd = 3)
        lines(pp.data(main.dist.failure.probabilities[, 1], log.of.data),
            quant(upper.ci, main.distribution), col = map.colors(4),
            lty = 3, lwd = 3)
    }
    if (do.legend) {
        if (band.type != "none") {
            legend(x = c(x.loc(0.003), x.loc(0.3)), y = c(y.loc(0.996),
                y.loc(0.7)), c(paste(distribution.name(main.distribution),
                "Distribution ML Fit"), other.fit, ci.description),
                lty = c(1, 2, 3), lwd = c(2, 3, 2), col = c(map.colors(1),
                  map.colors(2), map.colors(4)), bty = "n",y.intersp = 0.675)
}       else {
            legend(x = c(x.loc(0.003), x.loc(0.3)), y = c(y.loc(0.996),
                y.loc(0.7)), c(paste(distribution.name(main.distribution),
                "Distribution ML Fit"), other.fit), lty = c(1,
                2), lwd = c(2, 3), col = c(map.colors(1), map.colors(2)),
                bty = "n",y.intersp = 0.675)
        }
    }
    f.plot.censored.ticks(data.ld, log.of.data, plot.censored.ticks)
    oldClass(parametric.list) <- "multiple.mlest.out"
    invisible(parametric.list)
}
