#' @export
plot.use.rate.series.system <-
function (x, xlim = c(30, 4000), ylim = c(0.01,
    0.95), number.points = 100, lwd.fhat = 3, col.field = 6,
    lty.field = 1, my.title = NULL,...)
{
    use.rate.model <- x$use.rate.model
    ylab <- GetSMRDDefault("SMRD.LabelOnYaxis")
    lab.parameters <- use.rate.model$lab.parameters
    distributions <- use.rate.model$distributions
    use.rate.parameters <- use.rate.model$use.rate.parameters
    xlab <- "Days"
    distribution.cycles <- distributions[1]
    log.of.data <- is.logdist(distribution.cycles)
    if (log.of.data) {
        time.vec <- logseq(xlim[1], xlim[2], length = number.points)
        tran.time.vec <- log(time.vec)
}   else {
        time.vec <- seq(xlim[1], xlim[2], length = number.points)
        tran.time.vec <- time.vec
    }
    if (T) {
        npprobplot(x$field.data.ld, distribution = distribution.cycles,
            band.type = "none", pch = 1, col.points = 1, point.cex = 1,
            xlab = xlab, xlim = xlim, ylim = ylim)
}   else {
        probplot.setup(distribution.cycles, xlim = xlim,
            ylim = ylim, xlab = xlab, ylab = ylab)
    }
    all.modes <- use.rate.model$failure.modes
    modes.to.plot <- all.modes
    for (i in modes.to.plot) {
        mode.index <- match(modes.to.plot[i], all.modes)
        the.mode <- all.modes[mode.index]
        indices <- (2 * mode.index - 1):(2 * mode.index)
        mu.use.rate <- use.rate.parameters[indices[1]]
        sigma.use.rate <- use.rate.parameters[indices[2]]
        mu.cycles <- lab.parameters[indices[1]]
        sigma.cycles <- lab.parameters[indices[2]]
        distribution.cycles <- distributions[indices[1]]
        distribution.use.rate <- distributions[indices[2]]
        fhat.field <- use.rate.phib2(tran.time.vec, mu.use.rate,
            sigma.use.rate, mu.cycles, sigma.cycles)
        lines(pp.data(time.vec, log.of.data), pp.quant(fhat.field,
            distribution.cycles), col = mode.index, lwd = 2,
            lty = mode.index)
    }
    TheFieldParameters <- FieldParameters(use.rate.model)
    cat("Field parameters=", format(unlist(TheFieldParameters)),
        "\n")
    testrho <- (use.rate.model$rho * use.rate.parameters["Crack.sigma.use.rate"] *
        use.rate.parameters["Wear.sigma.use.rate"])/(TheFieldParameters$Field.sigmaWear *
        TheFieldParameters$Field.sigmaCrack)
    cat("rhoRR=", use.rate.model$rho, TheFieldParameters$rhoTT,
        testrho, "\n")
    fhat.field <- 1 - bvn(mu1 = TheFieldParameters$Field.muWear,
        mu2 = TheFieldParameters$Field.muCrack, sd1 = TheFieldParameters$Field.sigmaWear,
        sd2 = TheFieldParameters$Field.sigmaCrack, rho = TheFieldParameters$rhoTT,
        ah = tran.time.vec, ak = tran.time.vec)
    lines(pp.data(time.vec, log.of.data), pp.quant(fhat.field,
        distribution.cycles), col = col.field, lwd = 5, lty = 1)
    if (!is.null(my.title))
        mtext(text = my.title, line = 1)
    legend(x.loc(0.003), y.loc(0.996), c(modes.to.plot, "Combined"),
        cex = 1.1, bty = "n", col = c(1:length(modes.to.plot),
            col.field), lty = c(1:length(modes.to.plot), 1),
        lwd = 2, y.intersp = 0.675)
    invisible()
}
