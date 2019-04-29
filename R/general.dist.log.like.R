general.dist.log.like <-
function (thetain)
{
    logcheck <- function (x) { ifelse(x > 0, logb(x), -1e+21) }
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    f.origparam <- model$f.origparam
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 1) || map.SMRDDebugLevel() >=
        5)
        cat("in general.dist.log.like itr=", iter.count, paste(model$t.param.names,
            collapse = " "), "=", paste(format(thetain), collapse = " "),
            "\n")
    theta.origparam <- f.origparam(thetain, model)
    if (any(is.na(theta.origparam)))
        return(1e+10)
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 1) || map.SMRDDebugLevel() >=
        10) {
        cat("in general.dist.log.like", paste(model$orig.param.names,
            collapse = " "), "=", paste(format(theta.origparam),
            collapse = " "), "\n")
    }
    data.ld <- get(envir = .frame0,  "data.ld")
    z <-Response(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 7 && interactive()) ||
        (map.SMRDDebugLevel() > 8 && interactive()))
        browser()
    if (any(the.censor.codes == 1))
        fail.part <- sum(the.case.weights[the.censor.codes ==
            1] * logcheck(dgenmax(z[the.censor.codes == 1, 1],
            distribution = distribution, theta = theta.origparam)))
    if (any(the.censor.codes == 2))
        rcensor.part <- sum(the.case.weights[the.censor.codes ==
            2] * logcheck(1 - pgenmax(z[the.censor.codes == 2,
            1], distribution = distribution, theta = theta.origparam)))
    if (any(the.censor.codes == 3))
        lcensor.part <- sum(the.case.weights[the.censor.codes ==
            3] * logcheck(pgenmax(z[the.censor.codes == 2, 1],
            distribution = distribution, theta = theta.origparam)))
    if (any(the.censor.codes == 4))
        icensor.part <- sum(the.case.weights[the.censor.codes ==
            4] * (logcheck(pgenmax(z[the.censor.codes == 4, 2],
            distribution = distribution, theta = theta.origparam) -
            pgenmax(z[the.censor.codes == 4, 1], distribution = distribution,
                theta = theta.origparam))))
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 1) || map.SMRDDebugLevel() >=
        5)
        cat("in general.dist.log.like, likelihood=", paste(format(c(fail.part +
            rcensor.part + lcensor.part + icensor.part, fail.part,
            rcensor.part, lcensor.part, icensor.part)), collapse = " "),
            "\n")
    if ((iter.count < 4 && map.SMRDDebugLevel() >= 7 && interactive()) ||
        (map.SMRDDebugLevel() > 8 && interactive()))
        browser()
    answer <- -(fail.part + rcensor.part + lcensor.part + icensor.part)
    return(answer)
}
