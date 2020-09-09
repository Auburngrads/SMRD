gets.log.like <-
function (thetain)
{
    logcheck <- function (x) { ifelse(x > 0, logb(x), -1e+21) }
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0, inherits = !TRUE,"iter.count", iter.count )
    model <- get(envir = .frame0,  "model")
    f.origparam <- model$f.origparam
    distribution <- model$sub.distribution
    logtp1 <- thetain[1]
    logtp2 <- thetain[2]
    sigma <- thetain[3]
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 1) || map.SMRDDebugLevel() >
        4)
        print(paste("in gets.log.like", paste(model$t.param.names,
            collapse = " "), "=", paste(format(thetain), collapse = " ")))
    theta.origparam <- f.origparam(thetain, model)
    alpha <- theta.origparam[1]
    sigma <- theta.origparam[2]
    varzeta <- theta.origparam[3]
    if (varzeta < 1e-05)
        return(1e+10)
    data.ld <- get(envir = .frame0,  "data.ld")
    z <-Response(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    if (any(the.censor.codes == 1))
        fail.part <- sum(the.case.weights[the.censor.codes ==
            1] * dlgets(z[the.censor.codes == 1, 1], alpha, sigma,
            varzeta, distribution = distribution))
    if (any(the.censor.codes == 2))
        rcensor.part <- sum(the.case.weights[the.censor.codes ==
            2] * logcheck(sgets(z[the.censor.codes == 2, 1],
            alpha, sigma, varzeta, distribution = distribution)))
    if (any(the.censor.codes == 3))
        lcensor.part <- sum(the.case.weights[the.censor.codes ==
            3] * logcheck(pgets(z[the.censor.codes == 3, 1],
            alpha, sigma, varzeta, distribution = distribution)))
    if (any(the.censor.codes == 4))
        icensor.part <- sum(the.case.weights[the.censor.codes ==
            4] * (logcheck(pgets(z[the.censor.codes == 4, 2],
            alpha, sigma, varzeta, distribution = distribution) -
            pgets(z[the.censor.codes == 4, 1], alpha, sigma,
                varzeta, distribution = distribution))))
    loglikelihood <- (fail.part + rcensor.part + lcensor.part +
        icensor.part)
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 1) || map.SMRDDebugLevel() >=
        4) {
        print(paste("in gets.log.like", paste(model$orig.param.names,
            collapse = " "), "=", paste(format(c(alpha, sigma,
            varzeta)), collapse = " ")))
        print(paste("in gets.log.like, likelihood=", paste(format(c(loglikelihood,
            fail.part, rcensor.part, lcensor.part, icensor.part)),
            collapse = " ")))
    }
    return(Uminus(loglikelihood))
}
