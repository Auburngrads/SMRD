range.bvn.gmle.out <-
function (...,bvn.gmle.out, sigma.factor = 2.5, na.rm)
{
    mu.time <- bvn.gmle.out$origparam[1]
    sigma.time <- bvn.gmle.out$origparam[2]
    mu.dist <- bvn.gmle.out$origparam[3]
    sigma.dist <- bvn.gmle.out$origparam[4]
    rho <- bvn.gmle.out$origparam[5]
    xlim <- exp(c(mu.time - sigma.factor * sigma.time, mu.time +
        sigma.factor * sigma.time))
    ylim <- exp(c(mu.dist - sigma.factor * sigma.dist, mu.dist +
        sigma.factor * sigma.dist))
    return(list(xlim, ylim))
}
