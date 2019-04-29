plot.bands <-
function (x, log.of.data, distribution, shape = NULL, col.fhat = 1,
    col.ci = 4, cex.point = 1.5, lwd.ci = 3, lwd.fhat = 2,...)
{
    lower <- x$lower
    upper <- x$upper
    if (is.null(lower) || is.null(upper))
        stop("Incorrect input object---no lower and upper")
    if (!is.null(x$times)) {
        x.over <- x$x.over
        times <- x$times
        lines(pp.data(times, log.of.data), pp.quant(lower,
            distribution, shape), col = col.ci, lty = 3,
            lwd = lwd.ci)
        lines(pp.data(times, log.of.data), pp.quant(upper,
            distribution, shape), col = col.ci, lty = 3,
            lwd = lwd.ci)
        fhat <- x$fhat
        if (!is.null(fhat)) {
            lines(pp.data(times[x.over], log.of.data), pp.quant(fhat,
                distribution, shape)[x.over], col = col.fhat,
                lwd = lwd.fhat)
        }
  } else {
        cdfest.out <- x$cdfest.out
        at.point <- x$at.point
        over.interval <- x$over.interval
        if (is.null(cdfest.out) || is.null(at.point))
            stop("Incorrect input object")
        if (any(at.point)) {
            points.default(pp.data(cdfest.out$p[at.point], log.of.data),
                pp.quant(lower[at.point], distribution, shape),
                pch = "-", cex = (cex.point * GetSMRDDefault("SMRD.point.size"))/100,
                col = col.ci)
            points.default(pp.data(cdfest.out$p[at.point], log.of.data),
                pp.quant(upper[at.point], distribution, shape),
                pch = "-", cex = (cex.point * GetSMRDDefault("SMRD.point.size"))/100,
                col = col.ci)
        }
        if (any(over.interval)) {
            segments(pp.data(cdfest.out$p[over.interval], log.of.data),
                pp.quant(lower[over.interval], distribution,
                  shape), pp.data(cdfest.out$q[over.interval],
                  log.of.data), pp.quant(lower[over.interval],
                  distribution, shape), lty = 3, lwd = lwd.ci,
                col = col.ci)
            segments(pp.data(cdfest.out$p[over.interval], log.of.data),
                pp.quant(upper[over.interval], distribution,
                  shape), pp.data(cdfest.out$q[over.interval],
                  log.of.data), pp.quant(upper[over.interval],
                  distribution, shape), lty = 3, lwd = lwd.ci,
                col = col.ci)
        }
    }
    invisible()
}
