#' @export
plot.joint.prior <-
function (x, post.or.prior, type.position, newdata = NULL,
    logger = "none", number.plot = 500, xlim = c(NA, NA),
    ylim = c(NA, NA), my.title = NULL, include.likelihood = F,
    size = 4, factor = 4, axes.range.default.post = T, identify.points = F,...)
{
    distribution <- generic.distribution(x$distribution)
    data.ld <- x$data.ld
    sample.to.plot <- x[[post.or.prior]]
    length.sample <- nrow(sample.to.plot)
    plot.these <- 1:min(length.sample, number.plot)
    plot.map.out <- map.joint.sample(sample.to.plot, x$p,
        type.position, newdata, distribution)
    if (post.or.prior == "prior" || axes.range.default.post)
        range.map.out <- plot.map.out
    else range.map.out <- map.joint.sample(x[["prior"]],
        x$p, type.position, newdata, distribution)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(range.map.out$x.to.plot)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(range.map.out$y.to.plot)[yrna]
    plot.paper(xlim, ylim, x.axis = range.map.out$x.axis,
        y.axis = range.map.out$y.axis, grids = F, xlab = range.map.out$xlab,
        ylab = range.map.out$ylab)
    xvec <- plot.map.out$x.to.plot[plot.these]
    yvec <- plot.map.out$y.to.plot[plot.these]
    if (range.map.out$x.axis == "log")
        xvec <- logb(xvec)
    points.default(xvec, yvec, pch = 16, cex = (0.5 * GetSMRDDefault("SMRD.point.size"))/100)
    if (identify.points)
        identify(xvec, yvec, 1:length(yvec))
    can.do.likelihood <- T
    if (include.likelihood) {
        switch(type.position, Quantile = , quantile = {
            the.quantile <- newdata
            if (is.logdist(distribution)) {
                transformationx <- "log"
                log.quantile <- T
                xlim <- logb(xlim)
            } else {
                transformationx <- "linear"
                log.quantile <- F
            }
        }, Parameter = , parameter = {
            if (distribution == "weibull") {
                transformationx <- "log"
                xlim <- logb(xlim)
                log.quantile <- T
                the.quantile <- wqmf.phibf(0, distribution)
            } else {
                transformationx <- "linear"
                log.quantile <- F
                the.quantile <- NA
            }
        }, {
            cat(paste("\ncannot do likelihood contours for",
                type.position, "\n"))
            can.do.likelihood <- F
        })
        if (can.do.likelihood) {
            likelihood.grid.out <- simple.grid(data.ld, distribution,
                size = size, the.quantile = the.quantile, factor = factor,
                log.quantile = log.quantile, xlim = xlim,
                ylim = ylim, restrict.expand.range = T)
            profile.contour(likelihood.grid.out, transformationx = transformationx,
                transformationy = "linear", levels = c(0.01,
                  0.1, 0.5, 0.9), add = T)
        }
    }
    if (is.null(my.title)) {
        sample.name <- single.ifelse(post.or.prior == "post",
            "Posterior", "Prior")
        my.title <- paste(x$distribution, "Model",
            sample.name, "Distribution for", get.data.title(x$data.ld))
    }
    title(my.title, cex = 0.8)
}
