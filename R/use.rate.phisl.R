use.rate.phisl <-
function (field.yresp, mu.use.rate, sigma.use.rate, distribution.use.rate, 
    mu.cycles, sigma.cycles, distribution.cycles) 
{
    use.rate.distribution.table <- get.discrete.dist(distribution = distribution.use.rate, 
        mu = mu.use.rate, sigma = sigma.use.rate)
    pdf <- rep(0, length(field.yresp))
    x <- use.rate.distribution.table$x
    probs <- use.rate.distribution.table$probs
    tvec <- 1
    if (is.logdist(distribution.cycles)) 
        tvec <- exp(field.yresp)
    for (i in 1:length(x)) {
        z <- (field.yresp - (mu.cycles - logb(x[i])))/sigma.cycles
        pdf <- pdf + (probs[i] * wqmf.phis(z, distribution.cycles))/(tvec * 
            sigma.cycles)
    }
    return(logb(pdf))
}
