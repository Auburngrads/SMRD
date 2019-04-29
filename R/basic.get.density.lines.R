basic.get.density.lines <-
function (density.at, mu, sigma, distribution, scale.factor = 1, 
    x.axis) 
{
    q.low <- quant(0.01, distribution)
    q.high <- quant(0.99, distribution)
    zvec <- seq(q.low, q.high, length = 50)
    xden <- (.Uminus(scale.factor) * wqmf.phis(zvec, distribution))/sigma
    yden <- (mu + zvec * sigma)
    if (is.logdist(distribution)) 
        yden <- exp(yden)
    return(list(xden = xden, yden = yden, at = f.relationship(density.at, 
        x.axis)))
}
