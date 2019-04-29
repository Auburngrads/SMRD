tsplot.warranty.sequence <-
function (warranty.gmle.out.list, miles.limit = 36, time.limit = 36,
    max.number = NULL, plot.segments = T, my.title = NULL)
{
    period.list <- names(warranty.gmle.out.list)
    if (is.null(max.number))
        max.number <- length(warranty.gmle.out.list)
    failure.probability <- rep(NA, max.number)
    se.failure.probability <- rep(NA, max.number)
    lower.failure.probability <- rep(NA, max.number)
    upper.failure.probability <- rep(NA, max.number)
    for (i in 1:max.number) {
        warranty.gmle.out <- warranty.gmle.out.list[[period.list[i]]]
        if (!is.null(warranty.gmle.out$dummy) && warranty.gmle.out$dummy ==
            T) {
            failure.probability[i] <- 0
            se.failure.probability[i] <- 0
            lower.failure.probability[i] <- 0
            upper.failure.probability[i] <- 0
            next
        }
        mu.time <- warranty.gmle.out$origparam[1]
        sigma.time <- warranty.gmle.out$origparam[2]
        mu.dist <- warranty.gmle.out$origparam[3]
        sigma.dist <- warranty.gmle.out$origparam[4]
        rho <- warranty.gmle.out$origparam[5]
        failure.probability[i] <- bvnsw(mu.time, mu.dist, sigma.time,
            sigma.dist, rho, logb(time.limit), logb(miles.limit))
        vecpar.bvnsw <- function(origparam, log.time.limit, log.miles.limit) {
            mu.time <- origparam[1]
            sigma.time <- origparam[2]
            mu.dist <- origparam[3]
            sigma.dist <- origparam[4]
            rho <- origparam[5]
            return(bvnsw(mu.time, mu.dist, sigma.time, sigma.dist,
                rho, log.time.limit, log.miles.limit))
        }
        se.failure.probability[i] <- f.gendeltamethod(warranty.gmle.out$origparamvcv,
            warranty.gmle.out$origparam, vecpar.bvnsw, log.time.limit = logb(time.limit),
            log.miles.limit = logb(miles.limit))$se
        lower.failure.probability[i] <- failure.probability[i] +
            1.96 * se.failure.probability[i]
        upper.failure.probability[i] <- -1.96 * se.failure.probability[i]
    }
    probability.confidence.interval.out <- probability.confidence.interval(failure.probability,
        se.failure.probability)
    lower.failure.probability <- probability.confidence.interval.out$lower
    upper.failure.probability <- probability.confidence.interval.out$upper
    year <- as.numeric(substring(period.list[1], 6, 9))
    time.vec <- seq(year, by = 1/12, length = length(failure.probability))
    failure.probability <- ts(failure.probability, start = year,
        frequency = 12)
    lower.failure.probability <- ts(lower.failure.probability,
        start = year, frequency = 12)
    upper.failure.probability <- ts(upper.failure.probability,
        start = year, frequency = 12)
    ylim <- range(failure.probability, lower.failure.probability,
        upper.failure.probability)
    ts.plot(failure.probability, type = "b", pch = 1, ylim = ylim)
    points(lower.failure.probability, pch = 2)
    points(upper.failure.probability, pch = 6)
    if (plot.segments) {
        segments(time.vec, lower.failure.probability, time.vec,
            upper.failure.probability, lty = 3)
        if (is.null(my.title))
            my.title <- paste("Labor Code", warranty.gmle.out.list[[1]]$Lcode,
                "\nML estimate of fraction of cars with in-warranty reported failures for different build months")
        mtext(my.title, side = 3, line = 2)
    }
    invisible()
}
