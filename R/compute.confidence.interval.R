compute.confidence.interval <-
function (param, stderr, kodet, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
    interval.type = "pointwise", one.sided = F) 
{
    if (is.null(kodet)) 
        kodet <- 1
    kodet <- expand.vec(kodet, length(param))
    if (one.sided == "upper" || one.sided == "lower") {
        the.conf.level <- 1 - 2 * (1 - conf.level)
        if (interval.type == "simultaneous") 
            stop("One sided simultaneous not implemented")
    }
    else {
        if (!is.logical(one.sided)) 
            warning("in compute.POD.ahat.CV check one.sided")
        if (one.sided) 
            stop("problem with one-sided in compute.POD.ahat.CV ")
        the.conf.level <- conf.level
    }
    switch(interval.type, pointwise = {
        zquant <- sqrt(qchisq(conf.level, 1))
    }, simultaneous = {
        zquant <- sqrt(qchisq(conf.level, 2))
    }, {
        stop(paste(interval.type, "not recognized"))
    })
    lower <- rep(NA, length(param))
    upper <- rep(NA, length(param))
    the.ones <- kodet == 1
    if (any(the.ones)) {
        lower[the.ones] <- param[the.ones] - zquant * stderr[the.ones]
        upper[the.ones] <- param[the.ones] + zquant * stderr[the.ones]
    }
    the.ones <- kodet == 2
    if (any(the.ones)) {
        if (all(param[the.ones] > 0)) {
            lower[the.ones] <- param[the.ones]/exp((zquant * 
                stderr[the.ones])/(param[the.ones]))
            upper[the.ones] <- param[the.ones] * exp((zquant * 
                stderr[the.ones])/(param[the.ones]))
        }
        else {
            upper[the.ones] <- param[the.ones]/exp((zquant * 
                stderr[the.ones])/(-param[the.ones]))
            lower[the.ones] <- param[the.ones] * exp((zquant * 
                stderr[the.ones])/(-param[the.ones]))
        }
    }
    the.ones <- kodet == 3
    if (any(the.ones)) {
        stderrq <- stderr[the.ones]/(param[the.ones] * (1 - param[the.ones]))
        lower[the.ones] <- plogis(qlogis(param[the.ones]) - zquant * 
            stderrq)
        upper[the.ones] <- plogis(qlogis(param[the.ones]) + zquant * 
            stderrq)
    }
    if (!is.logical(one.sided)) {
        if (one.sided == "lower") 
            upper <- rep(NA, length(upper))
        if (one.sided == "upper") 
            lower <- rep(NA, length(lower))
    }
    return(list(fun.hat = param, se.fun = stderr, fun.lower = lower, 
        fun.upper = upper))
}
