ls3.log.like <-
function (thetain)
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    data.ld <- get(envir = .frame0,  "data.ld")
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    f.origparam <- model$f.origparam
    distribution <- get(envir = .frame0,  "model")$distribution
    theta.origparam <- f.origparam(thetain, model)
    if (is.null(model$xbar)) {
        mu <- theta.origparam[1]
        sigma <- theta.origparam[2]
  } else {
        mu <- theta.origparam[1] + theta.origparam[2] * xmat(data.ld)
        sigma <- theta.origparam[3]
    }
    print.iter <- (iter.count < 4 && map.SMRDDebugLevel() >=
        4) || map.SMRDDebugLevel() >= 5
    if (print.iter)
        cat("in ls3.log.like", iter.count, paste(names(theta.origparam),
            collapse = " "), "=", paste(format(theta.origparam),
            collapse = " "), "\n")
    if (print.iter)
        cat("in ls3.log.like", iter.count, paste(model$t.param.names,
            collapse = " "), "=", paste(format(thetain), collapse = " "),
            "\n")
    if (iter.count <= 0)
        browser()
    if (is.na(sigma) || sigma < 1e-05)
        return(1e+10)
    if  (is.even(numdist(distribution))) {
        yresp <- logb(Response(data.ld))
  } else {
        yresp <-Response(data.ld)
    }
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    the.truncation.codes <- truncation.codes(data.ld)
    if (!is.null(the.truncation.codes)) {
        if  (is.even(numdist(distribution))) {
            the.truncation.response <- logb(truncation.response(data.ld))
      } else {
            the.truncation.response <- truncation.response(data.ld)
        }
        ztrun <- (the.truncation.response - mu)/sigma
    }
    z <- (yresp - mu)/sigma
    fail.part <- 0
    rcensor.part <- 0
    lcensor.part <- 0
    icensor.part <- 0
    rtrun.part <- 0
    ltrun.part <- 0
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
    if (!is.null(the.truncation.codes)) {
        if (any(the.truncation.codes == 2))
            rtrun.part <- sum(the.case.weights[the.truncation.codes ==
                2] * logb(wqmf.phibf(ztrun[the.truncation.codes ==
                2, 1], distribution)))
        if (any(the.truncation.codes == 3))
            ltrun.part <- sum(the.case.weights[the.truncation.codes ==
                3] * wqmf.phibml(ztrun[the.truncation.codes ==
                3, 1], distribution))
    }
    the.likelihood <- fail.part + rcensor.part + lcensor.part +
        icensor.part - ltrun.part - rtrun.part
    return(.Uminus((the.likelihood)))
}
