multiple.probplot <-
function (data.list, data.ld.name, distribution, xlab, ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, xlim = c(NA,
        NA), ylim = c(NA, NA), dump = 1, grids = F, band.type = "Simultaneous",
    how.show.fhat = "points", how.show.interval = "step.fun",
    my.title = NULL, cex = 1.5, linear.axes = F, title.option = GetSMRDDefault("SMRD.TitleOption"),
    pch = (1:(length(do.list) + 1))[-2], ci.list = NULL, lwd = rep(1,
        length(do.list)), trunc.correct = T, col.fhat.vec = 1:length(do.list),
    col.ci = 6, shape = NULL, do.legend = "On plot", point.cex = 2,
    add.title = NULL, a.limit = 0.001, b.limit = 0.999, do.list = names(data.list),
    plotem = rep(T, length(do.list)), ...)
{
  default.title <- data.ld.name
    if (is.null(my.title)) {
        if (bands$band.type != "none") {
            my.title <- paste(default.title, "\n", "with Nonparametric ",
                bands$band.type, paste(floor(conf.level * 100 +
                  0.01), "%", sep = ""), "Confidence Bands")
}       else {
            my.title <- paste(default.title, "\n", "Nonparametric CDF Estimate")
        }
    }
    xlim.new <- NULL
    xrna <- is.na(xlim)
    if (any(xrna)) {
        for (i in 1:length(do.list)) {
            data.subset.d <- data.list[[do.list[i]]]
            xlim.new <- range(xlim.new, get.time.range(data.subset.d,
                distribution))
        }
    }
    xlim[xrna] <- xlim.new[xrna]
    bands.list <- list()
    cdpoints.list <- list()
    nonparametric.list <- list()
    ylim.data <- NULL
    plotted <- rep(F, length(do.list))
    for (i in 1:length(do.list)) {
        data.name <- do.list[i]
        data.subset.d <- data.list[[do.list[i]]]
        nonparametric.list[[data.name]] <- NA
        if (!good.data(data.subset.d)) {
            print(paste("Skipping", data.name, "because too few failures"))
            next
        }
        cdfest.out <- cdfest(data.subset.d)
        if (length(cdfest.out$q) <= 0) {
            print(paste("Skipping", data.name, "because cannot do cdfest"))
            next
        }
        plotted[i] <- T
        cdpoints.out <- cdpoints(cdfest.out)
        trunc.correct <- (!is.null(cdfest.out$left.trun.cond) ||
            !is.null(cdfest.out$right.trun.cond)) && trunc.correct
        mlest.out <- mlest(data.subset.d, distribution,...)
        if (trunc.correct)
        cdpoints.out <- truncadj(cdpoints.out, mlest.out)
        cdpoints.list[[data.name]] <- cdpoints.out
        nonparametric.list[[data.name]] <- cdfest.out
        if (is.onlist(data.name, names(data.list)[ci.list]))
            conf.level.send <- conf.level
        else conf.level.send <- 0
        bands <- get.npbands(cdfest.out, band.type, conf.level = conf.level,
            how.show.interval = how.show.interval, a.limit = a.limit,
            b.limit = b.limit)
        bands.list[[data.name]] <- bands
        ylim.data <- range(ylim.data, cdfest.out$prob[cdfest.out$prob >
            0], cdpoints.out$pplot, bands$fhat, strip.na(bands$lower),
            strip.na(bands$upper))
    }
    if (length(nonparametric.list) == 0)
        stop(paste("No estimable data sets in", data.ld.name))
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- ylim.data[yrna]
    log.of.data <- probplot.setup(distribution, xlim, ylim,
        my.title = my.title, xlab = xlab, ylab = ylab, grids = grids,
        linear.axes = linear.axes, title.option = title.option,
        cex = cex)
    for (i in 1:length(do.list)) {
        data.name <- do.list[i]
        cdpoints.out <- cdpoints.list[[data.name]]
        cdfest.out <- nonparametric.list[[data.name]]
        if (is.null(nonparametric.list[[data.name]]) || is.na(cdfest.out))
            next
        bands <- bands.list[[data.name]]
        times <- bands$times
        plot.nonparametric.estimate(cdfest.out = cdfest.out,
            cdpoints.out = cdpoints.out, distribution = distribution,
            log.of.data = log.of.data, how.show.fhat = how.show.fhat,
            shape = shape, point.cex = point.cex, pch = pch[i],
            xlim)
    }
    plotted <- plotted & plotem
    if (do.legend == "On plot" && any(plotted))
        legend(x.loc(0.003), y.loc(0.996), do.list[plotted],
            cex = 1.1, bty = "n", col = col.fhat.vec[plotted],
            pch = pch[plotted]%%19,y.intersp = 0.675)
    if ((do.legend == "New page" || do.legend == "New file") &&
        any(plotted)) {
        if (do.legend == "New file")
            postscript(file = "Save_legend.ps", horizontal = T)
        plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
            xaxt = "n", yaxt = "n")
        legend(x.loc(0.003), y.loc(0.997), do.list[plotted],
            cex = 1.1, bty = "n", col = col.fhat.vec[plotted],
            pch = pch[plotted]%%19,y.intersp = 0.675)
        if (do.legend == "New file")
            dev.off()
    }
    rlist <- list(nonparametric.list = nonparametric.list, cdpoints.list = cdpoints.list)
    oldClass(rlist) <- "multiple.cdfest.out"
    invisible(rlist)
}
