long.inner <-
function (tvec, r1log, mur1, sigmar1, mut2, sigmat2, mur2, sigmar2, 
    rho, distributionr2 = "lognormal", distributiont2 = "lognormal") 
{
    tlog <- log(tvec)
    mur2gr1 <- mur2 + rho * sigmar2 * ((r1log - mur1)/sigmar1)
    r2.distribution.table <- get.discrete.dist(distribution = distributionr2, 
        mu = mur2gr1, sigma = sigmar2)
    sf <- rep(0, length(tlog))
    x <- r2.distribution.table$x
    probs <- r2.distribution.table$probs
    for (i in 1:length(x)) {
        z <- (tlog - (mut2 + log(x[i])))/sigmat2
        sf <- sf + probs[i] * (1 - wqmf.phibf(z, distributiont2))
    }
    return(sf)
}
