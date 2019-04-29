basic.get.exp.trans.density.lines <-
function (trans.x.density.at, focus.variable, distribution, theta.hat, 
    scale.factor = 1, transformation.time, transformation.response, 
    transformation.x, FailLevel) 
{
    d.tau.d.time <- function(tau, transformation.time) {
        switch(generic.relationship.name(transformation.time), 
            squareroot = {
                return(1/(2 * tau))
            }, log = {
                return(1/(tau))
            }, linear = {
                return(1)
            }, {
                stop(paste("Unrecognized relationship=", 
                  transformation.time))
            })
    }
    x.axis <- subscript.relationship(transformation.x, focus.variable)
    beta2.names <- paste("beta", seq(2, length(transformation.x) + 
        1), sep = "")
    beta2.vec <- theta.hat[beta2.names]
    beta.x <- as.matrix(trans.x.density.at) %*% as.matrix(theta.hat[beta2.names], 
        ncol = 1)
    xfactor <- exp(-beta.x)/abs(theta.hat["beta1"])
    mu.density <- (theta.hat["beta0"] - f.relationship(FailLevel, 
        transformation.response)) * xfactor
    sigma.density <- theta.hat[length(theta.hat)] * xfactor
    increasing <- theta.hat["beta1"] > 0
    if (increasing) {
        spike.at.zero <- 1 - wqmf.phibf(-mu.density/sigma.density, 
            distribution)
        q.low <- quant(1 - ((1 - spike.at.zero) * 0.01 + spike.at.zero), 
            distribution)
        q.high <- quant(1 - ((1 - spike.at.zero) * 0.99 + spike.at.zero), 
            distribution)
        zvec <- seq(q.low, q.high, length = 50)
        tau.vec <- -(mu.density + zvec * sigma.density)
    }
    else {
        spike.at.zero <- wqmf.phibf(-mu.density/sigma.density, 
            distribution)
        q.low <- quant((1 - spike.at.zero) * 0.01 + spike.at.zero, 
            distribution)
        q.high <- quant((1 - spike.at.zero) * 0.99 + spike.at.zero, 
            distribution)
        zvec <- seq(q.low, q.high, length = 50)
        tau.vec <- mu.density + zvec * sigma.density
    }
    time.vec <- f.relationshipinv(tau.vec, transformation.time)
    the.d.tau.d.time <- d.tau.d.time(tau = tau.vec, transformation.time)
    if (increasing) {
        xden <- -exp(logb(time.vec) + wqmf.phisl((-tau.vec - 
            mu.density)/sigma.density, distribution) + logb(the.d.tau.d.time) - 
            logb(sigma.density))
    }
    else {
        xden <- -exp(logb(time.vec) + wqmf.phisl((tau.vec - mu.density)/sigma.density, 
            distribution) + logb(the.d.tau.d.time) - logb(sigma.density))
    }
    yden <- f.relationshipinv(tau.vec, transformation.time)
    return(list(xden = xden, yden = time.vec, at = trans.x.density.at[, 
        focus.variable], spike.at.zero = spike.at.zero))
}
