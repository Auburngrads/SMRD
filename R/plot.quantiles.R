#' @export
plot.quantiles <-
function (x, log.of.data, xrange = range(Response(x$data.ld)),
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, col.mle = 1,
    col.ci = 6, print.table = F, lty = 1, lwd = 1, timelen = 50,
    plotem = T, allow.extremes = F, mono.tran = F,...)
{
    distribution <- x$distribution
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- x$theta.hat
    sigma <- theta.hat[2]
    if (length(xrange) > 2) {
        if  (is.even(numdist(distribution))) {
            ltime <- logb(xrange)
            time <- exp(ltime)
      } else {
            ltime <- xrange
            time <- ltime
        }
  } else {
        if  (is.even(numdist(distribution))) {
            ltime <- seq(logb(xrange[1]), logb(xrange[2]), length = timelen)
            time <- exp(ltime)
      } else {
            ltime <- seq(xrange[1], xrange[2], length = timelen)
            time <- ltime
        }
    }
    if (generic.distribution(distribution) == "exponential") {
        evdistribution <- "Weibull"
  } else {
        evdistribution <- distribution
    }
    z <- (ltime - theta.hat[1])/sigma
    dist.probs <- wqmf.phibf(z, evdistribution)
    if (allow.extremes)
        zgood <- dist.probs >= 0 & dist.probs <= 1
    else zgood <- dist.probs > 0 & dist.probs < 1
    ltime <- ltime[zgood]
    time <- time[zgood]
    dist.probs <- dist.probs[zgood]
    z <- z[zgood]
    if (length(z) <= 0) {
        print("Probabilities out of bounds")
        return()
    }
    dist.quantiles <- quant(dist.probs, distribution)
    if (plotem)
        lines(pp.data(time, log.of.data), dist.quantiles, col = col.mle,
            lty = lty, lwd = lwd)
    zmat <- cbind(1, z)
    varz <- diag(zmat %*% x$vcv %*% t(zmat))/(sigma^2)
    stderror <- sqrt(varz) * wqmf.phis(z, evdistribution)
    stderrq <- stderror/(dist.probs * (1 - dist.probs))
    if (conf.level > 0.1) {
        if (mono.tran) {
        }
        lower <- plogis(qlogis(dist.probs) - zvalue * stderrq)
        upper <- plogis(qlogis(dist.probs) + zvalue * stderrq)
        if (mono.tran) {
            lower <- mono.lower(lower)
            upper <- mono.upper(upper)
        }
        if (plotem)
            lines(pp.data(time, log.of.data), quant(lower, distribution),
                col = col.ci, lty = 3, lwd = 2)
        if (plotem)
            lines(pp.data(time, log.of.data), quant(upper, distribution),
                col = col.ci, lty = 3, lwd = 2)
        the.table <- cbind(Time = time, Probability = dist.probs,
            StdError = stderror, Lower = mono.lower(lower), Upper = mono.upper(upper))
        if (print.table) {
            print(the.table)
        }
        return(the.table)
    }
    return(list(time = time, dist.probs = dist.probs, stderror = stderror))
}
