luis.failure.probability <-
function (xuse, critdeg, beta0, beta1, beta2, sigma, prob, TimeUnits, 
   ResponseUnits, at.time.list = c("0", "20", "100", "200", 
        "500", "1000")) 
{
    time.list <- as.numeric(at.time.list)
    yrange <- c(20, 200)
    x <- luis.arrhenius(xuse)
    TimeRange <- range(time.list)
    time <- sqrt(c(0, max(TimeRange)))
    mu <- matrix(beta0 + beta1 * time * exp(-beta2 * x), ncol = length(prob), 
        nrow = 2)
    temp <- matrix(sigma * qnorm(prob), nrow = 2, ncol = 2, byrow = T)
    Q <- exp(mu + temp)
    print(Q)
    plot(time, Q[, 1], type = "l", xlab = TimeUnits, ylab =ResponseUnits, 
        ylim = yrange, log = "y", xaxt = "n", xlim = c(-2, 1.1 * 
            sqrt(max(TimeRange))))
    lines(time, Q[, 2])
    axis(1, at = sqrt(time.list), labels = at.time.list)
    abline(h = critdeg)
    expys0 <- seq(min(yrange), max(yrange), length = 1500)
    ys0 <- log(expys0)
    at1800 <- sqrt(0)
    mu <- beta0 + at1800 * beta1 * exp(-beta2 * x)
    z <- (ys0 - mu)/sigma
    stdpdf <- dnorm(z)
    stdpdf <- (stdpdf - min(stdpdf))/max(stdpdf - min(stdpdf))
    scalpdf <- sqrt(stdpdf) * 0.05 * (max(time) - min(time))
    for (at in sqrt(time.list)) {
        temp <- exp(ys0 + beta1 * exp(-beta2 * x) * (at - at1800))
        select <- (temp >= min(yrange))
        expys <- temp[select]
        dens <- at - scalpdf[select]
        lines(dens, expys)
        index <- expys <= critdeg
        xcoor <- c(dens[index], dens[index][1], dens[index][1])
        ycoor <- c(expys[index], max(expys[index]), expys[index][1])
        polygon(xcoor, ycoor, density = -1)
        abline(v = at)
        text(1.05 * c(sqrt(max(TimeRange)), sqrt(max(TimeRange))), 
            Q[2, ], paste(100 * prob, "%", sep = ""))
    }
}
