quant.to.plan.values <-
function (t1, p1, t2, p2, distribution) 
{
    sigma <- (logb(t2) - logb(t1))/(quant(p2, distribution) - 
        quant(p1, distribution))
    mu <- logb(t1) - quant(p1, distribution) * sigma
    return(c(sigma = sigma, mu = mu, beta = 1/sigma, expmu = exp(mu), 
        z1 = (logb(t1) - mu)/sigma, z2 = (logb(t2) - mu)/sigma))
}
