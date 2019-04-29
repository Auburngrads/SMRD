fx.mfmc.quantiles <-
function (theta.hat, multiple.mlest.out, prob.vec, time.range, 
    number.times = 10) 
{
    distribution <- multiple.mlest.out[[1]]$distribution
    log.of.data <- is.logdist(distribution)
    the.times <- seq(time.range[1], time.range[2], length = number.times)
    the.probs <- fx.mfmc.probs(theta.hat, multiple.mlest.out, 
        the.times)
    good.ones <- the.probs > 0 & the.probs < 1
    quanhat <- approx(pp.quant(the.probs[good.ones], distribution), 
        pp.data(the.times[good.ones], log.of.data), pp.quant(prob.vec, 
            distribution), rule = 1)$y
    if (log.of.data) 
        quanhat <- exp(quanhat)
    if (F) {
        cat("\nthe.times=", paste(format(the.times), collapse = ","))
        cat("\nthe.probs=", paste(format(the.probs), collapse = ","))
        cat("\nfx.mfmc.quantiles answer=", paste(quanhat, collapse = ","), 
            "\n")
    }
    return(quanhat)
}
