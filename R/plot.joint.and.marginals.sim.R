#' @export
plot.joint.and.marginals.sim <-
function (x, focus.quantity1, focus.quantity.detail1,
    x.of.interest1 = NA, focus.quantity2, focus.quantity.detail2,
    x.of.interest2 = NA, number.points.plot = 500, xlim = c(NA,
        NA), ylim = c(NA, NA), plot.type = "histogram", my.title = NULL,
    xlab = NULL, ylab = NULL, ...)
{
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
        SMRD.NameOnPlot = "")
    old.par <- par(mfcol = c(2, 2), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    plot.joint.sim(x, focus.quantity1 = focus.quantity1,
        focus.quantity.detail1 = focus.quantity.detail1, x.of.interest1 = x.of.interest1,
        focus.quantity2 = focus.quantity2, focus.quantity.detail2 = focus.quantity.detail2,
        x.of.interest2 = x.of.interest2, number.points.plot = number.points.plot,
        xlim = xlim, ylim = ylim, my.title = "",
        xlab = xlab, ylab = ylab, cex = 0.8, ...)
    marginal.sample1 <- plot.marginals.sim(x, focus.quantity = focus.quantity1,
        focus.quantity.detail = focus.quantity.detail1, x.of.interest = x.of.interest1,
        my.title = "", plot.type = plot.type, xlab = xlab, cex = 0.8,
        ...)
    marginal.sample2 <- plot.marginals.sim(x, focus.quantity = focus.quantity2,
        focus.quantity.detail = focus.quantity.detail2, x.of.interest = x.of.interest2,
        my.title = "", plot.type = plot.type, xlab = ylab, cex = 0.8,
        ...)
    plot(c(0, 1), c(0, 1), bty = "n", xaxt = "n", yaxt = "n",
        type = "n", xlab = "", ylab = "")
    text(0.26, 0.85, "Mean of the simulation estimates", cex = 1.5,
        xpd = TRUE)
    marginal.label1 <- attr(marginal.sample1, "label")
    if (nchar(marginal.label1) > 25)
        marginal.label1 <- "Focus quantity 1"
    marginal.label2 <- attr(marginal.sample2, "label")
    if (nchar(marginal.label2) > 25)
        marginal.label2 <- "Focus quantity 2"
    text(-0.3, 0.6, paste(paste(marginal.label1, ":", sep = ""),
        format(mean(marginal.sample1), digits = 4)), adj = 0,
        cex = 1.25, xpd = TRUE)
    text(-0.3, 0.3, paste(paste(marginal.label2, ":", sep = ""),
        format(mean(marginal.sample2), digits = 4)), adj = 0,
        cex = 1.25, xpd = TRUE)
    sample.name <- ""
    if (is.null(my.title)) {
        my.title <- attr(x, "title")
    }
    if (is.R())
        title.line = -5
    else title.line = -1
    mtext(my.title, side = 3, line = title.line, outer = T, cex = 1.2)
    SMRDOptions(save.SMRD.options)
    invisible()
}
