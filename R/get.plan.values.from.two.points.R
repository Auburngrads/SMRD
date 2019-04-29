get.plan.values.from.two.points <-
function (distribution, times, probs, time.units = "Time")
{
    if (length(times) != 2)
        stop("times must list two times")
    if (length(probs) != 2)
        stop("probs must list two probabilities")
    if  (is.even(numdist(distribution)))
        ytimes <- logb(times)
    else ytimes <- times
    zc1 <- quant(probs[1], distribution)
    zc2 <- quant(probs[2], distribution)
    sigma <- (ytimes[1] - ytimes[2])/(zc1 - zc2)
    beta <- 1/sigma
    mu <- ytimes[1] - zc1 * sigma
    rlist <- list(distribution = distribution, mu = mu, sigma = sigma,
        zc1 = zc1, beta = beta, probs = probs, times = times,
        eta = exp(mu), time.units = time.units)
    oldClass(rlist) <- "plan.values"
    return(rlist)
}
