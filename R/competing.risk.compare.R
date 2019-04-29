competing.risk.compare <-
function (data1.ld, data2.ld, dataB.ld, distribution, distribution1 = NULL,
    distribution2 = NULL, xlab = "Time", time.vec = NULL, number = 100,
    shape = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    my.title = NULL, ...)
{
    distributionB <- distribution
    if (is.null(distribution1))
        distribution1 <- distribution
    if (is.null(distribution2))
        distribution2 <- distribution
    npprobplot(dataB.ld, distribution = distributionB, band.type = "",
        ...)
    mlest.outB <- mlest(dataB.ld, distribution = distributionB)
    mlest.out1 <- mlest(data1.ld, distribution = distribution1)
    mlest.out2 <- mlest(data2.ld, distribution = distribution2)
    log.of.data <- is.logdist(distributionB)
    if (is.null(time.vec)) {
        time.range <- range(get.time.range(mlest.out1), get.time.range(mlest.out2),
            get.time.range(mlest.outB))
        if (is.logdist(distributionB))
            time.vec <- logseq(time.range[1], time.range[2],
                length = number)
        else time.vec <- seq(time.range[1], time.range[2], length = number)
    }
    fhat1 <- get.parametric.bands(mlest.out1, conf.level = conf.level,
        xlim = time.vec)
    fhat2 <- get.parametric.bands(mlest.out2, conf.level = conf.level,
        xlim = time.vec)
    fhatB <- get.parametric.bands(mlest.outB, conf.level = conf.level,
        xlim = time.vec)
    times2 <- fhat2$times
    fhat1match <- approx(fhat1$times, fhat1$fhat, times2)
    where.na <- is.na(fhat1match$y)
    fmcdfest <- 1 - (1 - fhat1match$y[!where.na]) * (1 - fhat2$fhat[!where.na])
    lines(pp.data(times2[!where.na], log.of.data), pp.quant(fmcdfest,
        distribution, shape), lwd = 3, col = 6)
    lines(pp.data(fhat1$times, log.of.data), pp.quant(fhat1$fhat,
        distribution, shape), lty = 3, lwd = 2, col = 3)
    lines(pp.data(fhat2$times, log.of.data), pp.quant(fhat2$fhat,
        distribution, shape), lty = 3, lwd = 2, col = 4)
    lines(pp.data(fhatB$times, log.of.data), pp.quant(fhatB$fhat,
        distribution, shape), lty = 3, lwd = 2, col = 1)
}
