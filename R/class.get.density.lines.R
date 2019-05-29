class.get.density.lines <-
function (class.level, density.at, theta.hat, sigma, distribution, 
    scale.factor = 1, relationship, x.axis) 
{
    mu <- theta.hat[1] + sum(theta.hat[-c(1, length(theta.hat))] * 
        as.numeric(class.level))
    q.low <- quant(0.01, distribution)
    q.high <- quant(0.99, distribution)
    zvec <- seq(q.low, q.high, length = 50)
    xden <- (Uminus(scale.factor) * wqmf.phis(zvec, distribution))/sigma
    yden <- (mu + zvec * sigma)
    return(list(xden = xden, yden = yden, at = density.at))
}
