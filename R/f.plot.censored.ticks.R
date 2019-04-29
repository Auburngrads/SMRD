f.plot.censored.ticks <-
function (data.ld, log.of.data, plot.censored.ticks, col = 1)
{
    invisible()
    if (is.logical(plot.censored.ticks) && !plot.censored.ticks)
        return()
    if (is.logical(plot.censored.ticks) && plot.censored.ticks) {
        warning(paste(plot.censored.ticks, "Must specify top or bottom for plot.censored ticks"))
        return()
    }
    switch(plot.censored.ticks, top = {
        label.loc <- 1.055
        tick.loc <- c(1, 1.025)
    }, bottom = {
        label.loc <- 0.055
        tick.loc <- c(0, 0.025)
    }, {
        warning(paste(plot.censored.ticks, "Must specify top or bottom for plot.censored ticks"))
        return()
    })
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    old.par <- par(xpd = T)
    on.exit(par(old.par))
    the.censored <- the.censor.codes == 2
    censored.times <- (Response(data.ld))[the.censored, 1]
    unique.times <- unique(censored.times)
    the.weights <- the.case.weights[the.censored]
    if (sum(the.weights) <= 0)
        return()
    for (i in 1:length(unique.times)) {
        all.of.the <- censored.times == unique.times[i]
        total.number <- sum(the.weights[all.of.the])
        lines(pp.data(c(unique.times[i], unique.times[i]), log.of.data),
            y.loc(tick.loc), lwd = 1.5, col = col)

        if (total.number > 1)
            text(pp.data(unique.times[i], log.of.data), y.loc(label.loc),
                as.character(total.number), col = col, cex = 0.8)
    }
}
