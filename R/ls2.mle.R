ls2.mle <-
function (data.ld, distribution = "weibull",debug1= F, theta.start = NULL) 
{
    options(digits = 5)
    assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    special.stuff <- list(1)
    orig.param.names <- c("location", "scale")
    t.param.names <- c("location", "scale")
    model <- list(distribution = distribution, form = NULL)
    model$orig.param.names <- orig.param.names
    model$t.param.names <- t.param.names
    if (is.null(theta.start)) {
      theta.start <- mlest(data.ld, distribution)$theta.hat
    }
    gmle.out <- gmle(log.like = ls2.log.like, data.ld = data.ld, 
        theta.start = theta.start, model = model, special.stuff = special.stuff, 
        t.param.names = t.param.names)
    theta.hat <- gmle.out$est.out$x
    mu <- theta.hat[1]
    sigma <- exp(theta.hat[2])
    parameters <- list(mu = mu, sigma = sigma)
    gmle.out$parameters <- parameters
    return(gmle.out)
}
