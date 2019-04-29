probplot.mmr <-
function (results, number.to.plot = 20, distribution = "Weibull", 
    number.points = 4) 
{
    sample.size <- sum(attr(results, "censor.scheme")$number.units)
    censoring.times <- attr(results, "censor.scheme")$censor.times
    end.time <- censoring.times[length(censoring.times)]
    theta <- attr(results, "theta")
    ylim <- c(1/(3 * sample.size), wqmf.phibf((logb(end.time) - 
        theta[1])/theta[2], distribution))
    xlim <- exp(quant(ylim, distribution))
    probpaper(distribution = distribution, xlim = xlim, 
        ylim, grids = F, linear.axes = F, slope.axis = F, 
        my.title = "", sub.title = "", xlab = "Time")
    xvec <- seq(logb(xlim[1]), logb(xlim[2]), length = number.points)
    get.z <- function(theta.hat, xvec) {
        (xvec - theta.hat[1])/theta.hat[2]
    }
    if (number.to.plot < nrow(results)) 
        number.to.plot <- nrow(results)
    ymat.mle <- apply(results[1:number.to.plot, 1:2], 1, get.z, 
        xvec = xvec)
    ymat.mrr <- apply(results[1:number.to.plot, 3:4], 1, get.z, 
        xvec = xvec)
    lines(xvec, (xvec - theta[1])/theta[2], lwd = 4)
    matlines(xvec, ymat.mle, col = 5, lty = 1)
    matlines(xvec, ymat.mrr, col = 6, lty = 1)
}
