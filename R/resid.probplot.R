#' Title
#'
#' @param groupm.list 
#' @param xlim 
#' @param ylim 
#' @param original.par 
#' @param my.title 
#' @param point.cex 
#' @param shape 
#' @param title.option 
#' @param grids 
#' @param conf.level 
#' @param band.type 
#' @param a.limit 
#' @param b.limit 
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
#' resid.probplot(ZelenCap.groupm.out1)
#' 
#' }
resid.probplot <-
function (groupm.list, xlim = c(NA, NA), ylim = c(NA, NA),
    original.par = T, my.title = NULL, point.cex = 1.2, shape = NULL,
    title.option = GetSMRDDefault("SMRD.TitleOption"), grids = 0, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    band.type = "Simultaneous", a.limit = 0.001, b.limit = 0.999,
    ...)
{
    groupm.out <- extract.results(groupm.list)
    positive.case.weights <- case.weights(groupm.out$data.ld) >
        0
    if (is.logdist(groupm.out$distribution)) {
        if (any(groupm.out$residuals[positive.case.weights, ] ==
            0)) {
            warning("Zero residuals----probably means bad convergence---check starting values\n\nNot making the requested residual probability plot")
            return()
        }
    }
    residuals.ld <- make.frame.ld(y = groupm.out$residuals[positive.case.weights,
        ], the.censor.codes = censor.codes(groupm.out$data.ld)[positive.case.weights],
        the.case.weights = case.weights(groupm.out$data.ld)[positive.case.weights])
    if (is.null(my.title)) {
        band.string <- paste(" with", percent.conf.level(conf.level),
            band.type, "Confidence Bands")
        group.var <- groupm.out$group.var
        analysis.type <- groupm.out$analysis.type
        my.title <- paste(get.data.title(groupm.out$data.ld),
            "\n", analysis.type, " Residual Probability Plot",
            band.string, "\n", paste(get.xlabel(groupm.out$data.ld)[group.var],
                name.relationship(groupm.out$relationship, allow = T),
                sep = "", collapse = ", "), paste(", Dist:",
                groupm.out$distribution, sep = ""), sep = "")
    }
    npprobplot(residuals.ld, distribution = groupm.out$distribution,
        xlab = "Standardized Residuals", xlim = xlim, ylim = ylim,
        band.type = band.type, conf.level = conf.level, a.limit = a.limit,
        b.limit = b.limit, my.title = my.title, point.cex = point.cex,
        grids = grids, title.option = title.option, trunc.correct = T,
        ylab = "Probability", ...)
    invisible()
}
