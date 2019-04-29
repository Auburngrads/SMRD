use.rate.phib <-
function (field.yresp, mu.userate, sigma.userate, distribution.userate, 
    mu.cycles, sigma.cycles, distribution.cycles, prob.min = 0.001, 
    prob.max = 0.999, number.cut = 100) 
{
    use.rate.distribution.table <- get.discrete.dist(distribution = distribution.userate, 
        mu = mu.userate, sigma = sigma.userate, prob.min = prob.min, 
        prob.max = prob.max, number.cut = number.cut)
    cdf <- rep(0, length(field.yresp))
    x <- use.rate.distribution.table$x
    probs <- use.rate.distribution.table$probs
    for (i in 1:length(x)) {
        z <- (field.yresp - (mu.cycles - log(x[i])))/sigma.cycles
        cdf <- cdf + probs[i] * wqmf.phibf(z, distribution.cycles)
    }
    return(cdf)
}
