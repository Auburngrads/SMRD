get.parametric.bands.quant <-
function (mlest.out, xlim = range(Response(mlest.out$data.ld)), 
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, number.points = 50, 
    shape = NULL, need.list = T) 
{
    distribution <- mlest.out$distribution
    if (generic.distribution(distribution) == "exponential") {
        evdistribution <- "Weibull"
    }
    else {
        evdistribution <- distribution
    }
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- mlest.out$theta.hat
    sigma <- theta.hat[2]
    if (is.logdist(distribution)) {
        range.log.times <- log(xlim)
    }
    else {
        range.log.times <- xlim
    }
    z.range <- (range.log.times - theta.hat[1])/sigma
    z.vec <- seq(z.range[1], z.range[2], length = number.points)
    prob.vec <- wqmf.phibf(z.vec, evdistribution)
    prob.vec.too.big <- prob.vec >= 1
    if (any(prob.vec.too.big)) 
        prob.vec <- prob.vec[!prob.vec.too.big]
    prob.vec.too.small <- prob.vec <= 0
    if (any(prob.vec.too.small)) 
        prob.vec <- prob.vec[!prob.vec.too.small]
    the.table <- quantiles.mlest(mlest.out, prob.vec = prob.vec, 
        conf.level = conf.level)
    return(list(Quanhat = the.table[, "Quanhat"], p = the.table[, 
        "p"], stderror = the.table[, "Std.Err."], lower = the.table[, 
        4], upper = the.table[, 5], bands.over = rep(T, length(the.table[, 
        "Quanhat"]))))
}
