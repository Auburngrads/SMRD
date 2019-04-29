get.residuals.dest.degrad.mle.out <-
function (x,...)
{
    trans.data.ddd <- x$trans.data.ddd
    theResponse <-Response(trans.data.ddd)
    the.times <- times(trans.data.ddd)
    the.xmat <- xmat(trans.data.ddd)
    origparam <- x$origparam
    beta2.names <- paste("beta", seq(2, ncol(the.xmat) + 1),
        sep = "")
    beta0 <- origparam[1]
    beta1 <- origparam[2]
    beta2.vec <- origparam[beta2.names]
    beta.x <- as.matrix(the.xmat) %*% beta2.vec
    mu <- beta0 + beta1 * exp(beta.x) * the.times
    sigma <- origparam[length(origparam)]
    z <- (theResponse - mu)/sigma
    attr(z, "fitted.values") <- mu
    return(z)
}
