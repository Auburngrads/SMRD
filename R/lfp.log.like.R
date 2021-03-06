lfp.log.like <-
function (theta)
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0, inherits = !TRUE,"iter.count", iter.count )
    mu <- theta[1]
    sigma <- exp(theta[2])
    if (sigma < 1e-05)
        return(1e+10)
    plfp <- plogis(theta[3])
    data.ld <- get(envir = .frame0,  "data.ld")
    distribution <- get(envir = .frame0,  "model")$distribution
    yresp <- logb(Response(data.ld))
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    z <- (yresp - mu)/sigma
    fail.part <- sum(the.case.weights[the.censor.codes == 1] *
        (wqmf.phisl(z[the.censor.codes == 1], distribution) +
            logb(plfp) - logb(sigma)))
    censor.part <- sum(the.case.weights[the.censor.codes == 2] *
        logb(1 - plfp * wqmf.phibf(z[the.censor.codes == 2],
            distribution)))
    return(Uminus((fail.part + censor.part)))
}
