map.joint.sample <-
function (sample.to.plot, base.line.p, type.position, newdata, 
    distribution) 
{
    if (distribution == "weibull") {
        y.to.plot <- 1/sample.to.plot[, 2]
        ylab <- "beta"
    }
    else {
        y.to.plot <- sample.to.plot[, 2]
        ylab <- "sigma"
    }
    y.axis <- "linear"
    switch(type.position, quantile = , Quantile = {
        newp <- newdata
        if (newp == base.line.p) {
            x.to.plot <- sample.to.plot[, 1]
        } else {
            x.to.plot <- exp(logb(sample.to.plot[, 1]) + sample.to.plot[, 
                2] * (quant(newp, distribution) - quant(base.line.p, 
                distribution)))
        }
        xlab <- paste(newp, "quantile")
        if (is.logdist(distribution)) {
            x.axis <- "log"
        } else {
            x.axis <- "linear"
        }
    }, parameter = , Parameter = {
        newp <- wqmf.phibf(0, distribution)
        x.to.plot <- (logb(sample.to.plot[, 1]) + sample.to.plot[, 
            2] * (quant(newp, distribution) - quant(base.line.p, 
            distribution)))
        if (distribution == "weibull") {
            xlab <- "eta"
            x.axis <- "log"
            x.to.plot <- exp(x.to.plot)
        } else {
            xlab <- "mu"
            x.axis <- "linear"
        }
    }, `failure probability` = , `Failure probability` = {
        newp <- wqmf.phibf(0, distribution)
        x.to.plot <- (logb(sample.to.plot[, 1]) + sample.to.plot[, 
            2] * (quant(newp, distribution) - quant(base.line.p, 
            distribution)))
        x.to.plot <- wqmf.phibf((logb(newdata) - x.to.plot)/sample.to.plot[, 
            2], distribution)
        xlab <- paste("F(", newdata, ")", sep = "")
        x.axis <- "linear"
    }, `Base Quantile` = {
        x.to.plot <- sample.to.plot[, 1]
        xlab <- paste(base.line.p, "quantile")
        if (is.logdist(distribution)) {
            x.axis <- "log"
        } else {
            x.axis <- "linear"
        }
    }, {
        stop(paste("bad type.position", type.position))
    })
    return.list <- list(x.to.plot = x.to.plot, y.to.plot = y.to.plot, 
        x.axis = x.axis, y.axis = y.axis, xlab = xlab, ylab = ylab)
    return(return.list)
}
