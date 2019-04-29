mttf.integrate <-
function (theta, distribution, smallq = 1e-12, bigq = 1 - smallq) 
{
    FUN <- function(t, mu, sigma, distribution) {
        z1 <- (logb(t) - mu)/sigma
        return((1 - wqmf.phibf(z1, distribution)))
    }
    mu <- theta[1]
    sigma <- theta[2]
    if (!is.logdist(distribution)) 
        stop(paste(distribution, "is not a log distribution"))
    upper <- exp(mu + sigma * quant(bigq, distribution))
    result <- integrate(FUN, 0, upper, mu = mu, sigma = sigma, 
        distribution = distribution)
    return(result$value)
}
