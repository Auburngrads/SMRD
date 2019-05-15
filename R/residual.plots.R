#' Title
#'
#' @param results.out 
#' @param which.plot 
#' @param x.to.plot 
#' @param my.title 
#' @param xlim 
#' @param ylim 
#' @param fvquant 
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
#' 
#' ZelenCap.groupm.out3 <- 
#'   groupm.mleprobplot(ZelenCap.ld, 
#'                      distribution = "Lognormal", 
#'                      relationship = c("linear", "linear"), 
#'                      formula = Location ~ + g(volts) + g(celsius) + g(volts):g(celsius))
#' 
#' residual.plots(ZelenCap.groupm.out3)
#' 
#' }
residual.plots <-
function (results.out, which.plot = "All", x.to.plot = NULL, 
    my.title = NULL, xlim = c(NA, NA), ylim = c(NA, NA), 
    fvquant = 0.5) 
{
    if (is.onlist("gmle.out", oldClass(results.out))) {
        gmle.out <- results.out
        distribution <- gmle.out$model$distribution
        data.ld <- gmle.out$data.ld
        residuals <- get.residuals(gmle.out)
        fitted.values <- attr(residuals, "fitted.values")
        mlest.out <- list(distribution = distribution, fitted.values = fitted.values, 
            residuals = as.matrix(residuals), data.ld = data.ld, 
            relationship = gmle.out$relationship, analysis.type = gmle.out$analysis.type)
    }
    else mlest.out <- results.out
    for (i in 1:length(which.plot)) {
        switch(which.plot[i], resid.vs.fit = {
            resid.vs.fit(mlest.out, my.title = my.title, xlim = xlim, 
                ylim = ylim)
        }, resid.probplot = {
            resid.probplot(mlest.out, my.title = my.title, xlim = xlim, 
                ylim = ylim)
        }, resid.vs.order = {
            resid.vs.order(mlest.out, my.title = my.title, xlim = xlim, 
                ylim = ylim)
        }, resid.vs.explan = {
            resid.vs.explan.multiple(mlest.out, my.title = my.title, 
                x.to.plot = x.to.plot, xlim = xlim, ylim = ylim)
        }, All = {
            resid.vs.fit(mlest.out, my.title = my.title)
            resid.probplot(mlest.out, my.title = my.title)
            resid.vs.order(mlest.out, my.title = my.title)
            resid.vs.explan.multiple(mlest.out, x.to.plot = x.to.plot, 
                my.title = my.title)
        }, {
        })
    }
    invisible(mlest.out)
}
