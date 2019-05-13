#' @export
plot.marginal.prior <-
function (x, post.or.prior, logger = "none", type.position = "Parameter",
    newdata = "mu", number.plot = 500,...)
{
    sample.to.plot <- x[[post.or.prior]]
    distribution <- generic.distribution(x$distribution)
    switch(type.position, quantile = , Quantile = {
        newp <- newdata
        if (is.logdist(distribution)) {
            priorx <- logb(sample.to.plot[, 1]) + sample.to.plot[,
                2] * (quant(newp, distribution) - quant(x$p,
                distribution))
            priorx <- (priorx)
            xlab <- paste("log(", newp, " quantile)", sep = "")
        } else {
            priorx <- sample.to.plot[, 1] + sample.to.plot[,
                2] * (quant(newp, distribution) - quant(x$p,
                distribution))
            xlab <- paste(newp, "quantile")
        }
    }, parameter = , Parameter = {
        switch(newdata, eta = , mu = {
            newp <- wqmf.phibf(0, distribution)
            if (is.logdist(distribution)) {
                priorx <- (logb(sample.to.plot[, 1]) + sample.to.plot[,
                  2] * (quant(newp, distribution) - quant(x$p,
                  distribution)))
            } else {
                priorx <- (sample.to.plot[, 1] + sample.to.plot[,
                  2] * (quant(newp, distribution) - quant(x$p,
                  distribution)))
            }
            priorx <- (logb(sample.to.plot[, 1]) + sample.to.plot[,
                2] * (quant(newp, distribution) - quant(x$p,
                distribution)))
            if (distribution == "weibull") {
                xlab <- "log(eta)"
                priorx <- (priorx)
            } else xlab <- "mu"
        }, spread = , beta = , sigma = {
            if (distribution == "weibull") {
                priorx <- 1/sample.to.plot[, 2]
                xlab <- paste("beta")
            } else {
                priorx <- sample.to.plot[, 2]
                xlab <- paste("sigma")
            }
        }, {
            stop("bad which parameter in new data")
        })
    }, `failure probability` = , `Failure probability` = {
        newp <- wqmf.phibf(0, distribution)
        priorx <- (logb(sample.to.plot[, 1]) + sample.to.plot[,
            2] * (quant(newp, distribution) - quant(x$p,
            distribution)))
        priory <- sample.to.plot[, 2]
        priorx <- wqmf.phibf((logb(newdata) - priorx)/priory,
            distribution)
        xlab <- paste("F(", newdata, ")", sep = "")
    }, {
        stop(paste("bad type.position", type.position))
    })
    xlim <- range(priorx)
    length.prior <- length(priorx)
    plot.these <- order(runif(length.prior))[1:min(length.prior,
        number.plot)]
    plot.these <- 1:length.prior
    switch(logger, x = {
        relationshipx <- "log"
        xvec <- logb(priorx[plot.these])
    }, {
        relationshipx <- "linear"
        xvec <- (priorx[plot.these])
    })
    marginal.trim.range <- range(trim.vector(priorx, trim = 0.005))
    x.width <- (2 * (marginal.trim.range[2] - marginal.trim.range[1]))/(logb(length(priorx),
        base = 2) + 1)
    den.out <- density(priorx, , width = x.width)
    hist(priorx, xlab = xlab, prob = T, cex = 1.5, yaxt = "n",
        col = 4)
    lines(den.out, lwd = 3)
}
