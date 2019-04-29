get.density.lines <-
function (density.at, theta.hat, sigma, distribution, scale.factor = 1, 
    relationship, x.axis) 
{
    mu <- theta.hat[1] + theta.hat[2] * f.relationship(density.at, 
        relationship)
    basic.get.density.lines(density.at = density.at, mu = mu, 
        sigma = sigma, distribution = distribution, scale.factor = scale.factor, 
        x.axis = x.axis)
}
