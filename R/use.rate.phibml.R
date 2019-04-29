use.rate.phibml <-
function (field.yresp, mu.use.rate, sigma.use.rate, distribution.use.rate, 
    mu.cycles, sigma.cycles, distribution.cycles) 
{
    use.rate.distribution.table <- get.discrete.dist(distribution = distribution.use.rate, 
        mu = mu.use.rate, sigma = sigma.use.rate)
    cdf <- rep(0, length(field.yresp))
    x <- use.rate.distribution.table$x
    probs <- use.rate.distribution.table$probs
    for (i in 1:length(x)) {
        z <- (field.yresp - (mu.cycles - log(x[i])))/sigma.cycles
        cdf <- cdf + probs[i] * wqmf.phibf(z, distribution.cycles)
    }
    return(log(1 - cdf))
}
