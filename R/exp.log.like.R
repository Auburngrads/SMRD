exp.log.like <-
function (x)
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    mu <- x[1]
    data.ld <- get(envir = .frame0,  "data.ld")
    yresp <- logb(Response(data.ld))
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    z <- (yresp - mu)
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    if (any(the.censor.codes == 1))
        fail.part <- sum(the.case.weights[the.censor.codes ==
            1] * (wqmf.phisl(z[the.censor.codes == 1, 1], "sev")))
    if (any(the.censor.codes == 2))
        rcensor.part <- sum(the.case.weights[the.censor.codes ==
            2] * wqmf.phibml(z[the.censor.codes == 2, 1], "sev"))
    if (any(the.censor.codes == 3))
        lcensor.part <- sum(the.case.weights[the.censor.codes ==
            3] * logb(wqmf.phibf(z[the.censor.codes == 3, 1],
            "sev")))
    if (any(the.censor.codes == 4))
        icensor.part <- sum(the.case.weights[the.censor.codes ==
            4] * (log.min(wqmf.phibf(z[the.censor.codes == 4,
            2], "sev") - wqmf.phibf(z[the.censor.codes == 4,
            1], "sev"))))
    return(Uminus((fail.part + rcensor.part + lcensor.part +
        icensor.part)))
}
