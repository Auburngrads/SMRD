ls.log.like <-
function (thetain)
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    data.ld <- get(envir = .frame0,  "data.ld")
   debug1<- get(envir = .frame0,  "debug1")
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    f.origparam <- model$f.origparam
    distribution <- get(envir = .frame0,  "model")$distribution
    theta.origparam <- f.origparam(thetain, model)
    mu <- theta.origparam[1]
    sigma <- theta.origparam[2]
    if ((iter.count < 4 &&debug1> 1) ||debug1> 4)
        print(paste("in ls.log.like", iter.count, paste(model$t.param.names,
            collapse = " "), "=", paste(format(thetain), collapse = " ")))
    if (sigma < 1e-05)
        return(1e+10)
    if  (is.even(numdist(distribution))) {
        yresp <- logb(Response(data.ld))
  } else {
        yresp <-Response(data.ld)
    }
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    z <- (yresp - mu)/sigma
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
    return(Uminus((fail.part + rcensor.part + lcensor.part +
        icensor.part)))
}
