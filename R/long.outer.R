long.outer <-
function (tvec, mut1, sigmat1, mut2, sigmat2, mur1, sigmar1, 
    mur2, sigmar2, rho, distributionr1 = "lognormal", distributiont1 = "lognormal", 
    prob.min = 1e-05, prob.max = 0.99999, number.cut = 900) 
{
    tlog <- log(tvec)
    r1.distribution.table <- get.discrete.dist(distribution = distributionr1, 
        mu = mur1, sigma = sigmar1, prob.min = prob.min, prob.max = prob.max, 
        number.cut = number.cut)
    rclike <- rep(0, length(tlog))
    x <- r1.distribution.table$x
    probs <- r1.distribution.table$probs
    for (i in 1:length(x)) {
        csf <- cprobur(tvec, r1log = log(x[i]), mut2, sigmat2, 
            mur1, sigmar1, mur2, sigmar2, rho)
        mut1gr1 <- mut1 - log(x[i])
        zt1gr1 <- (tlog - mut1gr1)/sigmat1
        den.t1gr1 <- dnorm(zt1gr1)/(sigmat1)
        rclike <- rclike + probs[i] * csf * den.t1gr1
    }
    return(log(rclike))
}
