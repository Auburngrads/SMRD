.f.ADDT.stableparam <-
function (beta, model) 
{
    beta2.names <- paste("beta", seq(2, length(model$xbar) + 
        1), sep = "")
    beta2.vec <- beta[beta2.names]
    if (F) {
        cat("beta2.vec=", beta2.vec, "\n")
        cat("beta2.names=", beta2.names, "\n")
        cat("beta=", beta, "\n")
        cat("names(beta)=", names(beta), "\n")
        cat("beta2.vec=", beta2.vec, "\n")
        cat("model$xbar=", model$xbar, "\n")
    }
    beta.x <- sum(beta2.vec * model$xbar)
    gamma0 <- beta[1] + beta[2] * model$tbar * exp(beta.x)
    gamma2.vec <- beta2.vec
    gamma1 <- beta[2] * exp(beta.x)
    sigma <- logb(beta[length(beta)])
    thetastable <- c(gamma0, gamma1, gamma2.vec, sigma)
    names(thetastable) <- model$stable.param.names
    return(thetastable)
}
