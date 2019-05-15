#' Title
#'
#' @param groupm.list 
#' @param original.par 
#' @param cex 
#' @param my.title 
#' @param grids 
#' @param xlim 
#' @param ylim 
#' @param x.axis 
#' @param x.to.plot 
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
resid.vs.explan <-
function (groupm.list, original.par = T, cex = 1, my.title = NULL,
    grids = F, xlim = c(NA, NA), ylim = c(NA, NA), x.axis = "linear",
    x.to.plot = NULL, cex.points = 1, ...)
{
    groupm.out <- extract.results(groupm.list)
    distribution <- groupm.out$distribution
    old.par <- par(mar = c(5, 6, 4, 2) + 0.1)
    if (original.par)
        on.exit(par(old.par))
    if  (is.even(numdist(distribution))) {
        yline.test05 <- exp(quant(0.05, distribution))
        yline.test50 <- exp(quant(0.5, distribution))
        yline.test95 <- exp(quant(0.95, distribution))
        y.axis <- "log"
        residuals <- logb(groupm.out$residuals)
  } else {
        yline.test05 <- quant(0.05, distribution)
        yline.test50 <- quant(0.5, distribution)
        yline.test95 <- quant(0.95, distribution)
        y.axis <- "linear"
        residuals <- groupm.out$residuals
    }
    if (!is.null(groupm.out$the.orig.data.ld)) {
        xmat <- xmat(groupm.out$the.orig.data.ld)
  } else {
        xmat <- xmat(groupm.out$data.ld)
    }
    number.obs <- nrow(xmat)
    if (is.null(x.to.plot)) {
        if (ncol(xmat) == 1)
            x.to.plot <- 1
        else stop("Must specify the x variable to plot")
    }
    xnames <- colnames(xmat)
    names(xnames) <- xnames
    if (length(x.to.plot) > 1)
        stop(paste("length of x.to.plot", paste(x.to.plot, collapse = ","),
            "is", length(x.to.plot)))
    if (is.numeric(x.to.plot)) {
        if (x.to.plot > ncol(xmat))
            stop(paste(x.to.plot, "is larger than the number of columns in the x matrix"))
  } else {
        if (is.onlist(x.to.plot, xnames))
            stop(paste(x.to.plot, "is not in the x matrix"))
    }
    the.censor.codes <- censor.codes(groupm.out$data.ld)
    the.case.weights <- case.weights(groupm.out$data.ld)
    dummy <- the.censor.codes == 0 | the.case.weights == 0
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(groupm.out$residuals[!dummy])[yrna]
    xrna <- is.na(xlim)
    the.x.vector <- xmat[, x.to.plot]
    if (is.factor(the.x.vector)) {
        if (!all(xrna))
            warning("No control of x-axis with class variable")
        xlim <- c(0, 1)
        plot.paper(xlim = xlim, ylim = ylim, x.axis = "blank",
            y.axis = y.axis, xlab = xnames[x.to.plot], ylab = "Standardized Residuals",
            cex = cex, my.title = "", cex.labs = 1.2, grids = grids,
            ...)
        the.x.vector <- factor(as.character(the.x.vector))
        unique.group.names <- levels(the.x.vector)
        density.at <- (1:length(unique.group.names) - 0.5)/(length(unique.group.names))
        axis(side = 1, at = density.at, labels = as.character(unique.group.names),
            adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex = 1.2)
        the.x.pos <- (as.numeric(the.x.vector) - 0.5)/length(unique.group.names)
  } else {
        if (any(xrna))
            xlim[xrna] <- range(xmat[, x.to.plot])[xrna]
        plot.paper(xlim, ylim, x.axis, y.axis, ylab = "Standardized Residuals",
            xlab = xnames[x.to.plot], cex = cex, cex.labs = 1.2,
            grids = grids, ...)
        the.x.pos <- the.x.vector
    }
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
        analysis.type <- groupm.out$analysis.type
        my.title <- paste(get.data.title(groupm.out$data.ld),
            "\n", analysis.type, " Residuals versus ", xnames[x.to.plot],
            "\n", paste(get.xlabel(groupm.out$data.ld)[group.var],
                name.relationship(relationship, allow = T),
                sep = "", collapse = ", "), paste(", Dist:",
                distribution, sep = ""), sep = "")
    }

    mtext(text = my.title, line = 0.5)
    ncensored <- the.censor.codes == 1 & !dummy
    rcensored <- the.censor.codes == 2 & !dummy
    lcensored <- the.censor.codes == 3 & !dummy
    icensored <- the.censor.codes == 4 & !dummy
    if (any(ncensored))
        points.default(the.x.pos[ncensored], residuals[ncensored,
            1], pch = 16, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(rcensored))
        points.default(the.x.pos[rcensored], residuals[rcensored,
            1], pch = 2, cex = (1.5 * cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(lcensored))
        points.default(the.x.pos[lcensored], residuals[lcensored,
            1], pch = 6, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(icensored))
        points.default(the.x.pos[icensored], (residuals[icensored,
            1] + groupm.out$residuals[icensored, 2])/2, pch = 0,
            cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    invisible()
}
