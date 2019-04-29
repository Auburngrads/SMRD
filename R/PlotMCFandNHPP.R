PlotMCFandNHPP <-
function (data.rdu, form, xlab = paste("Age in", get.time.units(data.rdu)),
    ylab = "Mean Cumulative Failures", plot.seg = T, number.points = 200,
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, band.type = "p",
    theta.start = c(NA, NA), my.title = NULL)
{
    event <- events(data.rdu)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(data.rdu)
    RecurrTimes <- Times[CriticalEvent, 1]
    non.positive <- RecurrTimes <= 0
    if (any(non.positive)) {
        if (any(RecurrTimes < 0))
            stop("Negative event times not allowed in NHPP model")
        warning("Events at time 0 detected. Adding a small amount to make positi\nve.")
        unique.times <- sort(unique(RecurrTimes))
        if (length(unique.times) < 2)
            stop("All events times are zero")
        eps <- (unique.times[2] - unique.times[1])/10
        RecurrTimes[non.positive] <- RecurrTimes[non.positive] +
            eps
        Times[CriticalEvent, ] <- RecurrTimes
        times(data.rdu) <- Times
    }
    if (any(EndPoints)) {
        censor.time <- Times[EndPoints, 1]
    }
    else {
        censor.time <- max(Times)
    }
    number.failures <- length(RecurrTimes)
    sum.times <- sum(RecurrTimes)
    mcf.out <- mcf(data.rdu)
    if (any(is.na(mcf.out$VarHat)) || all(mcf.out$VarHat == 0))
        band.type <- "n"
    if (band.type == "p") {
        ci.title <- paste(paste("\nwith ", floor(conf.level *
            100 + 0.01), "%", "Nonparametric Confidence Intervals",
            sep = ""))
    }
    else ci.title <- NULL
    if (is.null(my.title))
        my.title <- paste("Mean Cumulative Function for", get.data.title(mcf.out),
            ci.title, "and", form, "NHPP ML estimate")
    plot.mcf(mcf.out, xlab = xlab, ylab = ylab, plot.seg = plot.seg,
        conf.level = conf.level, band.type = band.type, my.title = my.title)
    time.vec <- logseq(min(RecurrTimes), max(censor.time, Times),
        length = number.points)
    NHPP.mle.out <- NHPP.mle(data.rdu, form = form, theta.start = theta.start)
    NHPPmcfhat <- NHPP.MCFvec(time.vec, NHPP.mle.out$origparam,
        form)
    lines(time.vec, NHPPmcfhat, lty = 3, lwd = 2, col = 9)
    print(NHPP.mle.out)
    return(NHPP.mle.out)
}
