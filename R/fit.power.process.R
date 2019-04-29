fit.power.process <-
function (data.ld)
{
    the.censor.codes <- censor.codes(data.ld)
    theResponse <-Response(data.ld)
    fail.times <- theResponse[the.censor.codes == 1, 1]
    censor.time <- theResponse[the.censor.codes == 2, 1]
    number.failures <- length(fail.times)
    betahat <- number.failures/sum(logb(censor.time/fail.times))
    etahat <- censor.time/number.failures^(1/betahat)
    mcf.plot(data.ld)
    time.vec <- logseq(min(fail.times), censor.time, length = 200)
    mcfhat <- (time.vec/etahat)^betahat
    lines(time.vec, mcfhat, lwd = 2)
    return(c(betahat = betahat, etahat = etahat))
}
