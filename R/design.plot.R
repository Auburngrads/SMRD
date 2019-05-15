#' Title
#'
#' @param data.ld 
#' @param x.cols 
#' @param xlab 
#' @param ylab 
#' @param pch 
#' @param cex 
#'
#' @return NULL
#' @export
#'
#' @examples 
#' \dontrun{
#' 
#' Tantalum.ld <- frame.to.ld(tantalum, 
#'                            response.column = 1,
#'                            censor.column = 2, 
#'                            case.weight.column = 3, 
#'                            x.column = c(4, 5), 
#'                            data.title = "Tantalum Capacitor Data", 
#'                            time.units = "Hours", 
#'                            xlabel = c("Volts","DegreesC"))
#' 
#' summary(Tantalum.ld)
#' 
#' design.plot(Tantalum.ld)
#' 
#' }
design.plot <-
function (data.ld, x.cols = c(1, 2), xlab = get.x.columns(data.ld)[x.cols[1]], 
    ylab = get.x.columns(data.ld)[x.cols[2]], pch = 16, cex = 1.5) 
{
    vec1 <- xmat(data.ld)[, x.cols[1]]
    vec2 <- xmat(data.ld)[, x.cols[2]]
    plot.paper(range(vec1), range(vec2), grids = F, xlab = xlab, 
        ylab = ylab, cex = 1.5)
    points.default(vec1, vec2, pch = pch, cex = cex)
}
