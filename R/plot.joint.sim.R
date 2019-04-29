plot.joint.sim <-
function (x, focus.quantity1 = focus.quantity1,
    focus.quantity.detail1, x.of.interest1 = x.of.interest1,
    focus.quantity2 = focus.quantity2, focus.quantity.detail2,
    x.of.interest2 = x.of.interest2, number.points.plot = 500,
    xlim = c(NA, NA), ylim = c(NA, NA), my.title = NULL,
    xlab = NULL, ylab = NULL, cex = 1, identify.points = F, ...)
{
    marginal.sample1 <- marginalize.sim(x, focus.quantity = focus.quantity1,
        focus.quantity.detail = focus.quantity.detail1, x.of.interest = x.of.interest1,
        ...)
    if (is.null(xlab))
        xlab <- attr(marginal.sample1, "label")
    marginal.sample2 <- marginalize.sim(x, focus.quantity = focus.quantity2,
        focus.quantity.detail = focus.quantity.detail2, x.of.interest = x.of.interest2,
        ...)
    if (is.null(ylab))
        ylab <- attr(marginal.sample2, "label")
    plot.these <- 1:min(nrow(x), number.points.plot)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(marginal.sample1)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(marginal.sample2)[yrna]
    x.axis <- attr(marginal.sample1, "axis")
    y.axis <- attr(marginal.sample2, "axis")
    plot.paper(xlim, ylim, x.axis = x.axis, y.axis = y.axis,
        grids = F, xlab = xlab, ylab = ylab, cex = cex, cex.labs = cex,
        cex.tic.lab = cex)
    xvec <- marginal.sample1[plot.these]
    yvec <- marginal.sample2[plot.these]
    if (x.axis == "log")
        xvec <- logb(xvec)
    if (y.axis == "log")
        yvec <- logb(yvec)
    points.default(xvec, yvec, pch = 16, cex = (0.5 * GetSMRDDefault("SMRD.point.size"))/100)
    if (identify.points)
        text(xvec, yvec, as.character(plot.these))
    if (is.null(my.title)) {
        my.title <- attr(x, "title")
    }
    if (is.R())
        title.line = 0.5
    else title.line = 3.2999999
    mtext(text = my.title, line = title.line, side = 3)
}
