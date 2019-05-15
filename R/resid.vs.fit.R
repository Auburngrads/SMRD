#' Title
#'
#' @param groupm.list 
#' @param original.par 
#' @param cex 
#' @param my.title 
#' @param grids 
#' @param xlim 
#' @param ylim 
#' @param cex.points 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ZelenCap.ld <- frame.to.ld(zelencap,
#'                            response.column = 1,
#'                            censor.column = 2,
#'                            case.weight.column = 3,
#'                            x.columns = c(4, 5),
#'                            time.units = "Hours")
#' ZelenCap.groupm.out1 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "Weibull")
#' 
#' resid.vs.order(ZelenCap.groupm.out1)
#' 
#' resid.vs.fit(ZelenCap.groupm.out1)
#' 
#' resid.vs.explan(ZelenCap.groupm.out1,
#'                 x.to.plot = 1)
#' 
#' resid.vs.explan(ZelenCap.groupm.out1,
#'                 x.to.plot = 2)
#' 
#' }
resid.vs.fit <-
function (groupm.list, original.par = T, cex = 1, my.title = NULL,
    grids = F, xlim = c(NA, NA), ylim = c(NA, NA), cex.points = 1,
    ...)
{
    groupm.out <- extract.results(groupm.list)
    old.par <- par(mar = c(5, 6, 4, 2) + 0.1)
    distribution <- groupm.out$distribution
    if (original.par)
        on.exit(par(old.par))
    if  (is.even(numdist(distribution))) {
        yline.test05 <- exp(quant(0.05, distribution))
        yline.test50 <- exp(quant(0.5, distribution))
        yline.test95 <- exp(quant(0.95, distribution))
        relationshipx <- "log"
        relationshipy <- "log"
        fitted.values <- logb(groupm.out$fitted.values)
        residuals <- logb(groupm.out$residuals)
  } else {
        yline.test05 <- quant(0.05, distribution)
        yline.test50 <- quant(0.5, distribution)
        yline.test95 <- quant(0.95, distribution)
        relationshipx <- "linear"
        relationshipy <- "linear"
        fitted.values <- groupm.out$fitted.values
        residuals <- groupm.out$residuals
    }
    the.censor.codes <- censor.codes(groupm.out$data.ld)
    the.case.weights <- case.weights(groupm.out$data.ld)
    dummy <- the.censor.codes == 0 | the.case.weights == 0
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(groupm.out$fitted.values[!dummy])[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(groupm.out$residuals[!dummy])[yrna]
    plot.paper(xlim, ylim, relationshipx, relationshipy,
        ylab = "Standardized Residuals", xlab = "Fitted Values",
        cex = cex, cex.labs = 1.2, grids = grids, ...)
    if (ylim[2] > yline.test50) {
        abline(h = quant(0.5, distribution))
        text(x.loc(1.03), quant(0.5, distribution), ".50")
    }
    if (ylim[2] > yline.test05) {
        abline(h = quant(0.05, distribution))
        text(x.loc(1.03), quant(0.05, distribution), ".05")
    }
    if (ylim[2] > yline.test95) {
        abline(h = quant(0.95, distribution))
        text(x.loc(1.03), quant(0.95, distribution), ".95")
    }
    if (is.null(my.title)) {
        group.var <- groupm.out$group.var
        relationship <- groupm.out$relationship
        title.distribution <- groupm.out$distribution
        analysis.type <- groupm.out$analysis.type
        my.title <- paste(get.data.title(groupm.out$data.ld),
            "\n", analysis.type, " Residuals versus Fitted Values\n",
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
        points.default(fitted.values[ncensored], residuals[ncensored,
            1], pch = 16, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(rcensored))
        points.default(fitted.values[rcensored], residuals[rcensored,
            1], pch = 2, cex = (1.5 * cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(lcensored))
        points.default(fitted.values[lcensored], residuals[lcensored,
            1], pch = 6, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(icensored))
        points.default(fitted.values[icensored], (residuals[icensored,
            1] + groupm.out$residuals[icensored, 2])/2, pch = 0,
            cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    invisible()
}
