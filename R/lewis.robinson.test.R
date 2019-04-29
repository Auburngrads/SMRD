lewis.robinson.test <-
function (data.rdu)
{
    the.censor.codes <- casefold(events(data.rdu))
    failure.censor.names <- ClistToVec(get.failure.censor.names(data.rdu))
    is.event <- is.element(the.censor.codes, failure.censor.names)
    the.times <- times(data.rdu)
    the.failure.times <- sort(the.times[is.event, 1])
    censor.time <- the.times[the.censor.codes == "end", 1]
    if (length(censor.time) > 1) {
        warning("more than one End time")
        censor.time <- max(censor.time)
    }
    number.failures <- length(the.failure.times)
    zvalue <- (sum(the.failure.times) - 0.5 * number.failures *
        censor.time)/(censor.time * sqrt(number.failures/12))
    ir.times <- interarrival.times(data.rdu, replace.zero = F)[["Time"]]
    SD <- sqrt(var(ir.times))
    if (SD <= 0)
        stop("Nonpositive variance of interarrival times")
    snr <- mean(ir.times)/SD
    zvalue <- zvalue * snr
    pvalue <- 1 - pchisq(zvalue^2, 1)
    return(c(snr = snr, zvalue = zvalue, pvalue = pvalue))
}
