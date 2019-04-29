get.alt.plan.values.from.two.points <-
function (distribution, relationship, probs, accelvar, censor.time,
    beta, sigma, time.units = "Time", accelvar.units, power = NULL)
{
    relationship <- set.relationship.power(relationship, power)
    if (missing(accelvar.units)) {
        switch(relationship, Arrhenius = , arrhenius = {
            accelvar.units <- "Degrees C"
        }, humidity = {
            accelvar.units <- "RH"
        }, stop("Need to specify units for accelerating variable"))
    }
    if (missing(sigma)) {
        if (missing(beta)) {
            cat("Both sigma and beta missing. Assuming that beta=1 \n \n")
            sigma <- 1
        }
        sigma <- 1/beta
}   else {
        if (!(missing(beta))) {
            stop("Cannot specify both sigma and beta")
        }
    }
    mu1 <- logb(censor.time) - quant(probs[1], distribution) *
        sigma
    mu2 <- logb(censor.time) - quant(probs[2], distribution) *
        sigma
    x1 <- f.relationship(accelvar[1], relationship)
    x2 <- f.relationship(accelvar[2], relationship)
    beta1 <- (mu2 - mu1)/(x2 - x1)
    beta0 <- mu2 - beta1 * x2
    beta <- 1/sigma
    theta.vec <- c(beta0 = beta0, beta1 = beta1, sigma = sigma)
    rlist <- list(distribution = distribution, relationship = relationship,
        beta0 = beta0, beta1 = beta1, beta = beta, sigma = sigma,
        probs = probs, accelvar = accelvar, censor.time = censor.time,
        accelvar.units = accelvar.units, theta.vec = theta.vec,
        time.units = time.units)
    oldClass(rlist) <- "alt.plan.values"
    return(rlist)
}
