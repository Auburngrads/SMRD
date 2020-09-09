egeng.log.like <-
function (thetain)
{
    logcheck <- function (x) { ifelse(x > 0, logb(x), -1e+21) }
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0, inherits = !TRUE,"iter.count", iter.count )
    model <- get(envir = .frame0,  "model")
    f.origparam <- model$f.origparam
    if (any(thetain == Inf) || (iter.count < 4 && map.SMRDDebugLevel() >=
        1) || map.SMRDDebugLevel() >= 4)
        print(paste("in egeng.log.like0 iter=", iter.count, paste(model$t.param.names,
            collapse = " "), "=", paste(format(thetain), collapse = " ")))
    theta.origparam <- f.origparam(thetain, model)
    mu <- theta.origparam[1]
    sigma <- theta.origparam[2]
    lambda <- theta.origparam[3]
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 5) || map.SMRDDebugLevel() >=
        10)
        browser()
    if (sigma < 1e-05)
        return(1e+10)
    data.ld <- get(envir = .frame0,  "data.ld")
    z <- logb(Response(data.ld))
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    if (any(the.censor.codes == 1))
        fail.part <- sum(the.case.weights[the.censor.codes ==
            1] * dlegengl(z[the.censor.codes == 1, 1], mu, sigma,
            lambda))
    if (any(the.censor.codes == 2))
        rcensor.part <- sum(the.case.weights[the.censor.codes ==
            2] * segengl(z[the.censor.codes == 2, 1], mu, sigma,
            lambda))
    if (any(the.censor.codes == 3))
        lcensor.part <- sum(the.case.weights[the.censor.codes ==
            3] * logcheck(pegengl(z[the.censor.codes == 3, 1],
            mu, sigma, lambda)))
    if (any(the.censor.codes == 4))
        icensor.part <- sum(the.case.weights[the.censor.codes ==
            4] * pdlegeng(z[the.censor.codes == 4, ], mu, sigma,
            lambda))
    the.loglike <- fail.part + rcensor.part + lcensor.part +
        icensor.part
    if ((iter.count < 4 && map.SMRDDebugLevel() > 1) || map.SMRDDebugLevel() >=
        4)
        print(paste("in egeng.log.like1", paste(format(c(mu,
            sigma, lambda, the.loglike)), collapse = " ")))
    if (map.SMRDDebugLevel() >= 5)
        print(paste("in egeng.log.like2", paste(format(c(fail.part,
            rcensor.part, lcensor.part, icensor.part)), collapse = " ")))
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 5) || map.SMRDDebugLevel() >=
        10)
        browser()
    return(Uminus((the.loglike)))
}
