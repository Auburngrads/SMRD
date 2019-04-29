.f.ADDT.origparam <-
function (gamma, model) 
{
    gamma2.indices <- seq(3, length(model$xbar) + 2)
    gamma2.vec <- gamma[gamma2.indices]
    beta2.vec <- gamma2.vec
    beta.x <- sum(beta2.vec * model$xbar)
    beta0 <- gamma[1] - gamma[2] * model$tbar
    beta1 <- gamma[2] * exp(-beta.x)
    sigma <- exp(gamma[length(gamma)])
    thetaorig <- c(beta0, beta1, beta2.vec, sigma)
    names(thetaorig) <- model$orig.param.names
    return(thetaorig)
}
