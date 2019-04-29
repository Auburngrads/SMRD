failure.probabilities.plan.values<-
function (x, time.vec = NULL, digits = 3, number.times = 10,...)
{
    old <- options(digits = digits)
    on.exit(options(old))
    distribution <- x$distribution
    log.of.data <- is.logdist(distribution)
    my.title <- paste("\n\nFailure probabilities from  the ",
        distribution, " distribution\n", sep = "")
    if (is.null(time.vec)) {
        time.range <- x$mu + quant(c(0.001, 0.999),
            distribution) * x$sigma
        if (log.of.data)
            time.range <- exp(time.range)
        if (log.of.data)
            time.vec <- as.numeric(logax(time.range)$ticlab)
        else time.vec <- as.numeric(linax(time.range)$ticlab)
    }
    if (log.of.data) {
        zvalue <- (logb(time.vec) - x$mu)/x$sigma
    }
    else {
        zvalue <- (time.vec - x$mu)/x$sigma
    }
    fail.probs <- wqmf.phibf(zvalue, distribution)
    the.table <- cbind(time.vec, fail.probs)

    colnames(the.table)[1] <- x$time.units
    cat(my.title)
    print(the.table)
    invisible(the.table)
}
