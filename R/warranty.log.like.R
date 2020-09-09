warranty.log.like <-
function (thetain)
{
    assign(envir = .frame0, inherits = !TRUE,"iter.count", value = 0)
    data.ld <- get(envir = .frame0,  "data.ld")
   debug1<- get(envir = .frame0,  "debug1")
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    f.origparam <- model$f.origparam
    distribution <- get(envir = .frame0,  "model")$distribution
    theta.origparam <- f.origparam(thetain, model)
    mu.time <- theta.origparam[1]
    sd.time <- theta.origparam[2]
    mu.dist <- theta.origparam[3]
    sd.dist <- theta.origparam[4]
    rho <- theta.origparam[5]
    if (sd.time < 1e-05 || sd.dist < 1e-05)
        return(1e+10)
    dist.resp <- logb(Response(data.ld))
    time.resp <- logb(xmat(data.ld)[, 1, drop = F])
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    fail.part <- 0
    rcensor.part <- 0
    if (any(the.censor.codes == 1))
        fail.part <- sum(the.case.weights[the.censor.codes ==
            1] * (dbvnl(time.resp[the.censor.codes == 1, 1],
            dist.resp[the.censor.codes == 1, 1], mu.time, mu.dist,
            sd.time, sd.dist, rho)))
    if (any(the.censor.codes == 2))
        rcensor.part <- sum(the.case.weights[the.censor.codes ==
            2] * (log.min(pbvnswc(time.resp[the.censor.codes ==
            2, 1], dist.resp[the.censor.codes == 2, 1], mu.time,
            mu.dist, sd.time, sd.dist, rho))))
    the.loglike <- fail.part + rcensor.part
    if ((iter.count < 4 &&debug1> 1) ||debug1>= 3)
        print(paste("in warranty.log.like", paste(format(c(the.loglike,
            mu.time, sd.time, mu.dist, sd.dist, rho)), collapse = " ")))
    if (debug1>= 4)
        print(paste("in warranty.log.like", paste(format(c(the.loglike,
            fail.part, rcensor.part)), collapse = " ")))
    return(-the.loglike)
}
