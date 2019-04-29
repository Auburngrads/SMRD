resid.vs.order <-
function (groupm.list, original.par = T, cex = 1, my.title = NULL,
    grids = F, xlim = c(NA, NA), ylim = c(NA, NA), cex.points = 1,
    ...)
{
    groupm.out <- extract.results(groupm.list)
    old.par <- par(mar = c(5, 6, 4, 2) + 0.1)
    if (original.par)
        on.exit(par(old.par))
    if  (is.even(numdist(groupm.out$distribution))) {
        yline.test <- 1
        y.axis <- "log"
        residuals <- logb(groupm.out$residuals)
  } else {
        yline.test <- 0
        y.axis <- "linear"
        residuals <- groupm.out$residuals
    }
    the.censor.codes <- censor.codes(groupm.out$data.ld)
    the.case.weights <- case.weights(groupm.out$data.ld)
    dummy <- the.censor.codes == 0 | the.case.weights == 0
    order.vec <- 1:length(residuals)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(order.vec)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(groupm.out$residuals[!dummy])[yrna]
    plot.paper(xlim, ylim, "linear", y.axis, ylab = "Standardized Residuals",
        xlab = "Observation Number", cex = cex, cex.labs = 1.2,
        grids = grids, ...)
    if (ylim[2] > yline.test)
        abline(h = 0)
    if (is.null(my.title)) {
        group.var <- groupm.out$group.var
        relationship <- groupm.out$relationship
        title.distribution <- groupm.out$distribution
        analysis.type <- groupm.out$analysis.type
        my.title <- paste(get.data.title(groupm.out$data.ld),
            "\n", analysis.type, " Residuals versus Observation Number\n",
            paste(get.xlabel(groupm.out$data.ld)[group.var],
                name.relationship(relationship, allow = T),
                sep = "", collapse = ", "), paste(", Dist:",
                title.distribution, sep = ""), sep = "")
    }

    mtext(text = my.title, line = 0.5)
    ncensored <- the.censor.codes == 1 & !dummy
    rcensored <- the.censor.codes == 2 & !dummy
    lcensored <- the.censor.codes == 3 & !dummy
    icensored <- the.censor.codes == 4 & !dummy
    if (any(ncensored))
        points.default(order.vec[ncensored], residuals[ncensored,
            1], pch = 16, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(rcensored))
        points.default(order.vec[rcensored], residuals[rcensored,
            1], pch = 2, cex = (1.5 * cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(lcensored))
        points.default(order.vec[lcensored], residuals[lcensored,
            1], pch = 6, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(icensored))
        points.default(order.vec[icensored], (residuals[icensored,
            1] + groupm.out$residuals[icensored, 2])/2, pch = 0,
            cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    lines(order.vec, residuals, lwd = 2)
    invisible()
}
