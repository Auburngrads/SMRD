cdf.all <-
function (f.cdfhat, real.time, gmle.out, conlev = 0.95) 
{
    ltail <- 1 - (1 - conlev)/2
    distribution <- gmle.out$model$distribution
    theta.hat <- gmle.out$est.out$x
    cdfhat <- f.cdfhat(theta.hat, real.time, distribution)
    logit.prob <- qlogis(cdfhat)
    delta <- 1e-04 * sqrt(diag(gmle.out$vcv))
    gradmat <- matrix(0, nrow = length(real.time), ncol = length(delta))
    for (pnumber in 1:length(delta)) {
        theta.hatm <- gmle.out$est.out$x
        theta.hatm[pnumber] <- theta.hatm[pnumber] - delta[pnumber]
        theta.hatp <- gmle.out$est.out$x
        theta.hatp[pnumber] <- theta.hatp[pnumber] + delta[pnumber]
        gradmat[, pnumber] <- (qlogis(f.cdfhat(theta.hatp, real.time, 
            distribution)) - qlogis(f.cdfhat(theta.hatm, real.time, 
            distribution)))/(2 * delta[pnumber])
    }
    sd.logit.prob <- sqrt(diag(gradmat %*% gmle.out$vcv %*% t(gradmat)))
    return(list(cdfhat = cdfhat, cdflower = plogis(logit.prob - 
        qnorm(ltail) * sd.logit.prob), cdfupper = plogis(logit.prob + 
        qnorm(ltail) * sd.logit.prob)))
}
