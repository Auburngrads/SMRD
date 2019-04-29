plot.bands.quantile <-
function (x, log.of.data, distribution, shape = NULL, col.fhat = 1,
    col.ci = 4, cex.point = 1.5, lwd.ci = 3, lwd.fhat = 2, mono.tran = T,...)
{
    lower <- x$lower
    upper <- x$upper
    if (is.null(lower) || is.null(upper))
        stop("Incorrect input object---no lower and upper")
    if (!is.null(x$Quanhat)) {
        x.over <- x$x.over
        p <- x$p
        if (mono.tran) {
            lower.plot.x <- mono.lower(lower[x.over])
            upper.plot.x <- mono.upper(upper[x.over])
        }
        else {
            lower.plot.x <- lower[x.over]
            upper.plot.x <- upper[x.over]
        }
        lines(pp.data(lower.plot.x, log.of.data), pp.quant(p,
            distribution, shape)[x.over], col = col.ci, lty = 3,
            lwd = lwd.ci)
        lines(pp.data(upper[x.over], log.of.data), pp.quant(p,
            distribution, shape)[x.over], col = col.ci, lty = 3,
            lwd = lwd.ci)
        if (!is.null(x$Quanhat[x.over])) {
            lines(pp.data(x$Quanhat[x.over], log.of.data),
                pp.quant(p, distribution, shape)[x.over],
                col = col.fhat, lwd = lwd.fhat)
        }
    }
    else {
    }
    invisible()
}
