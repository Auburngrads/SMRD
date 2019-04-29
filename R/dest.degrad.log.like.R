dest.degrad.log.like <-
function (thetain) 
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
   debug1<- get(envir = .frame0,  "debug1")
    model <- get(envir = .frame0,  "model")
    trans.data.ddd <- get(envir = .frame0,  "data.ld")
    theResponse <-Response(trans.data.ddd)
    the.xmat <- xmat(trans.data.ddd)
    the.censor.codes <- censor.codes(trans.data.ddd)
    the.times <- times(trans.data.ddd)
    the.case.weights <- case.weights(trans.data.ddd)
    f.origparam <- model$f.origparam
    distribution <- model$distribution
    if ((iter.count < 4 &&debug1> 4) ||debug1> 12) 
        cat(paste("in dest.degrad.log.like", paste(model$t.param.names, 
            collapse = ","), "=", paste(format(thetain), collapse = ",")), 
            "\n")
    theta.origparam <- f.origparam(thetain, model)
    beta0 <- theta.origparam[1]
    beta1 <- theta.origparam[2]
    beta2.names <- paste("beta", seq(2, length(model$xbar) + 
        1), sep = "")
    beta2.vec <- theta.origparam[beta2.names]
    sigma <- theta.origparam[length(theta.origparam)]
    if (is.na(sigma) || sigma < 1e-10) 
        return(1e+10)
    beta.x <- the.xmat %*% beta2.vec
    mu <- beta0 + beta1 * exp(beta.x) * the.times
    z <- (theResponse - mu)/sigma
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    if (any(the.censor.codes == 1)) 
        fail.part <- sum(the.case.weights[the.censor.codes == 
            1] * (-logb(sigma) + wqmf.phisl(z[the.censor.codes == 
            1, 1], distribution)))
    if (any(the.censor.codes == 2)) 
        rcensor.part <- sum(the.case.weights[the.censor.codes == 
            2] * wqmf.phibml(z[the.censor.codes == 2, 1], distribution))
    if (any(the.censor.codes == 3)) 
        lcensor.part <- sum(the.case.weights[the.censor.codes == 
            3] * logb(wqmf.phibf(z[the.censor.codes == 3, 1], 
            distribution)))
    if (any(the.censor.codes == 4)) 
        icensor.part <- sum(the.case.weights[the.censor.codes == 
            4] * (log.min(wqmf.phibf(z[the.censor.codes == 4, 
            2], distribution) - wqmf.phibf(z[the.censor.codes == 
            4, 1], distribution))))
    the.likelihood <- fail.part + rcensor.part + lcensor.part + 
        icensor.part
    if ((iter.count < 4 &&debug1> 2) ||debug1> 4) {
        cat(paste("in dest.degrad.log.like=", format(the.likelihood), 
            paste(model$t.param.names, collapse = ","), "=", 
            paste(format(thetain), collapse = ",")), "\n")
    }
    return(.Uminus(the.likelihood))
}
