get.plan.values.from.point.and.slope <-
function (distribution, beta, sigma, prob, time, time.units = "Time")
{
    if  (is.even(numdist(distribution)))
        time <- logb(time)
    if (missing(beta)) {
        if (missing(sigma)) {
            stop("Must specify either sigma or beta")
        }
        beta <- 1/sigma
  } else {
        if (missing(sigma)) {
            sigma <- 1/beta
      } else {
            stop("Cannot specify both sigma and beta")
        }
    }
    if (missing(prob) || missing(time))
        stop("Must specify both prob and time")
    if (missing(distribution))
        stop("Must specify distribution")
    zc1 <- quant(prob, distribution)
    mu <- time - zc1 * sigma
    if (is.logdist(distribution)) {
        time <- exp(time)
        eta <- exp(mu)
  } else {
        eta <- NA
        beta <- NA
    }
    rlist <- list(distribution = distribution, mu = mu, sigma = sigma,
        zc1 = zc1, beta = beta, probs = prob, times = time, eta = eta,
        time.units = time.units)
    oldClass(rlist) <- "plan.values"
    return(rlist)
}
