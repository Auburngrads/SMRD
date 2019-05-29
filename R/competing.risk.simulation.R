competing.risk.simulation <-
function (mu1 = logb(400), sig1 = 0.3, mu2 = logb(500), sig2 = 1.5,
    n = 30, p = 1, censor.time = 300, distribution = "Weibull",
    write.data = T, one.plot = T, ...)
{
    data1 <- floor(rweibull(n, shape = 1/sig1) * exp(mu1)) +
        1
    data2 <- floor(rweibull(n, shape = 1/sig2) * exp(mu2)) +
        1
    time <- pmin(data1, data2, censor.time)
    codes1 <- rep(0, n)
    codes2 <- rep(0, n)
    codesb <- rep(0, n)
    mode <- rep(0, n)
    mode1 <- data1 <= data2 & time != censor.time
    mode2 <- data2 < data1 & time != censor.time
    censored <- time == censor.time
    codes1[censored | mode2] <- 2
    codes2[censored | mode1] <- 2
    codesb[censored] <- 2
    codes1[mode1] <- 1
    codes2[mode2] <- 1
    codesb[mode1 | mode2] <- 1
    mode[mode1] <- 1
    mode[mode2] <- 2
    data1.d <- make.frame.ld(y = time, the.censor.codes = codes1)
    data2.d <- make.frame.ld(y = time, the.censor.codes = codes2)
    dataB.d <- make.frame.ld(y = time, the.censor.codes = codesb)
    if (write.data) {
        data.matrix <- cbind(time = time, mode = mode, codes1 = codes1,
            codes2 = codes2, codesb = codesb)
        write.matrix(data.matrix, file = "crisk.dat", append = F)
    }
    if (one.plot)
        postscript(file = "tmp.ps", horizontal = T)
    tmp3 <- mleprobplot(dataB.d, distribution = distribution,
        original.par = F, ...)
    tmp1 <- mleprobplot(data1.d, distribution = distribution,
        ...)
    if (!one.plot)
        tmp2 <- mleprobplot(data2.d, distribution = distribution,
            ...)
    if (!one.plot)
        if (one.plot)
            dev.off()
    tmp23 <- mleprobplot(dataB.d, distribution = distribution,
        original.par = F, conf.level = 0.05, ...)
    tmp1match <- approx(tmp1[, "Time"], tmp1[, "Probability"],
        tmp2[, "Time"])
    where.na <- is.na(tmp1match$y)
    fmcdfest <- 1 - (1 - tmp1match$y[!where.na]) * (1 - tmp2[,
        "Probability"][!where.na])
    if (is.logdist(distribution)) {
        plot.times1 <- logb(tmp1[, "Time"])[!where.na]
    }
    else {
        plot.times1 <- (tmp1[, "Time"])[!where.na]
    }
    if (is.logdist(distribution)) {
        plot.times2 <- logb(tmp2[, "Time"])[!where.na]
    }
    else {
        plot.times2 <- (tmp2[, "Time"])[!where.na]
    }
    sizes <- c(length(plot.times2), length(fmcdfest), length(plot.times1),
        length(tmp1[, "Probability"][!where.na]))
    lines(plot.times2, quant(fmcdfest, distribution), lwd = 3)
    lines(plot.times1, quant(tmp1[, "Probability"][!where.na],
        distribution), lwd = 3, col = 3)
    lines(plot.times2, quant(tmp2[, "Probability"][!where.na],
        distribution), lwd = 3, col = 4)
}
