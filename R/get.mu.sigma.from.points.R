get.mu.sigma.from.points <-
function (times, probs, distribution)
{
    if  (is.even(numdist(distribution)))
        times <- logb(times)
    zc1 <- quant(probs[1], distribution)
    zc2 <- quant(probs[2], distribution)
    sigma <- (times[1] - times[2])/(zc1 - zc2)
    mu <- times[1] - zc1 * sigma
    musigma <- c(mu, sigma, zc1, zc2)
    names(musigma) <- c("mu", "sigma", "zc1", "zc2")
    return(musigma)
}
