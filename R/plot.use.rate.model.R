plot.use.rate.model <-
function (x, modes.to.plot, xlim = c(NA, NA),
    ylim = c(NA, NA), field.data.ld = NULL, lab.data.list = NULL,
    number.points = 50, lwd.fhat = 3, col.field = 6, my.title = NULL,
    xlab = NULL, ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
    cex.fact = 1, add = F, lty.field = 1, plot.points = T, plot.lab = T,...)
{
    if (!is.character(modes.to.plot))
        stop(paste("modes.to.plot must be a character string or vector of character strings=",
            modes.to.plot, "\n"))
    if (is.null(xlab))
        xlab <- "Lab Cycles/Field Time"
    lab.parameters <- x$lab.parameters
    distributions <- x$distributions
    use.rate.parameters <- x$use.rate.parameters
    if (missing(modes.to.plot)) {
        modes.to.plot <- names(x$lab.data.list)
    }
    names(modes.to.plot) <- modes.to.plot
    response.range <- range(Response(field.data.ld))
    for (i in modes.to.plot) response.range <- range(response.range,
       Response(lab.data.list[[i]]))
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- response.range[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- c(1e-05, 0.95)[yrna]
    all.modes <- x$failure.modes
    for (i in modes.to.plot) {
        mode.index <- match(modes.to.plot[i], all.modes)
        the.mode <- all.modes[mode.index]
        indices <- (2 * mode.index - 1):(2 * mode.index)
        cat("indices******", paste(indices), "\n")
        mu.use.rate <- use.rate.parameters[indices[1]]
        sigma.use.rate <- use.rate.parameters[indices[2]]
        mu.cycles <- lab.parameters[indices[1]]
        sigma.cycles <- lab.parameters[indices[2]]
        distribution.cycles <- distributions[indices[1]]
        distribution.use.rate <- distributions[indices[2]]
        if (is.null(my.title))
            my.title.here <- paste("Lab:", get.data.title(lab.data.list[[i]]),
                "\nField:", get.data.title(field.data.ld), "rhoTT=",
                format(FieldParameters(x)$rhoTT))
        else my.title.here <- my.title
        probplot.setup(distribution.cycles, title.option = GetSMRDDefault("SMRD.TitleOption"),
            xlim = xlim, ylim = ylim, xlab = xlab,
            ylab = ylab)
        if (plot.points && !is.null(field.data.ld)) {
            field.data.list <- mfm.to.ld(field.data.ld)
            npprobplot(field.data.list[[i]], distribution = distribution.cycles,
                band.type = "none", add = T, pch = 1, col.points = col.field,
                point.cex = cex.fact)
        }
        if (plot.points && !is.null(lab.data.list)) {
            npprobplot(lab.data.list[[i]], distribution = distribution.cycles,
                band.type = "none", add = T, pch = 4, col.points = 1,
                point.cex = cex.fact)
            log.of.data <- is.logdist(distribution.cycles)
            if (log.of.data) {
                time.vec <- logseq(xlim[1], xlim[2], length = number.points)
                tran.time.vec <- log(time.vec)
}           else {
                time.vec <- seq(xlim[1], xlim[2], length = number.points)
                tran.time.vec <- time.vec
            }
            fhat.field <- use.rate.phib2(tran.time.vec, mu.use.rate,
                sigma.use.rate, mu.cycles, sigma.cycles)
            lines(pp.data(time.vec, log.of.data), pp.quant(fhat.field,
                distribution.cycles), col = col.field, lwd = lwd.fhat,
                lty = lty.field)
            z <- (tran.time.vec - mu.cycles)/sigma.cycles
            fhat.lab <- wqmf.phibf(z, distribution.cycles)
            lines(pp.data(time.vec, log.of.data), pp.quant(fhat.lab,
                distribution.cycles), col = 1, lwd = lwd.fhat)
        }
        if (!add)
            legend(x.loc(0.055), y.loc(0.95), paste(the.mode,
                c("Laboratory", "Field")), bty = "n", pch = c(4,
                1), col = c(1, col.field), lty = c(1, 1), lwd = c(3,
                3), cex = cex.fact, y.intersp = 0.675)
        if (!add)
            mtext(my.title.here, line = 2, cex = cex.fact)
    }
    invisible(list(fhat.field = fhat.field, time.vec = time.vec))
}
