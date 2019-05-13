#' @export
plot.censored.data.points <-
function (x, y.data, censor.codes, x.axis, y.axis, cex.points = 1.2,
    response.on.yaxis = T, cex = 1.2, pch.point = 16, dummy = rep(F,
        length(x)), ...)
{
    rcensored <- censor.codes == 2 & !dummy
    ncensored <- censor.codes == 1 & !dummy
    lcensored <- censor.codes == 3 & !dummy
    icensored <- censor.codes == 4 & !dummy
    if (response.on.yaxis) {
        if (any(ncensored))
            points.default(as.vector(x[ncensored]), y.data[ncensored,
                1], pch = pch.point, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
        if (any(rcensored))
            points.default(as.vector(x[rcensored]), y.data[rcensored,
                1], pch = 2, cex = (1.2 * cex.points * GetSMRDDefault("SMRD.point.size"))/100)
        if (any(lcensored))
            points.default(as.vector(x[lcensored]), y.data[lcensored,
                1], pch = 6, cex = (1.2 * cex.points * GetSMRDDefault("SMRD.point.size"))/100)
        if (any(icensored))
            points.default(as.vector(x[icensored]), (y.data[icensored,
                1] + y.data[icensored, 2])/2, pch = 4, cex = (cex.points *
                GetSMRDDefault("SMRD.point.size"))/100)
  } else {
        if (any(ncensored))
            points.default(y.data[ncensored, 1], as.vector(x[ncensored]),
                pch = pch.point, cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
        if (any(rcensored)) {
            rh.eps <- 0.001
            rh.size <- 0.1
            if (is.R()) {
                triangles(y.data[rcensored, 1], x[rcensored],
                  size = rh.size, "right")
          } else {
                arrows(y.data[rcensored, 1] - rh.eps, as.vector(x[rcensored]),
                  y.data[rcensored, 1] + rh.eps, as.vector(x[rcensored]),
                  size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100,
                  open = FALSE, rel = FALSE)
            }
        }
        if (any(lcensored)) {
            rh.eps <- 0.001
            rh.size <- 0.1
            if (is.R()) {
                triangles(y.data[rcensored, 1], x[rcensored],
                  size = rh.size, "left")
          } else {
                arrows(y.data[lcensored, 1] + rh.eps, as.vector(x[lcensored]),
                  y.data[lcensored, 1] - rh.eps, as.vector(x[lcensored]),
                  size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100,
                  open = FALSE, rel = FALSE)
            }
        }
        if (any(icensored))
            points.default((y.data[icensored, 1] + y.data[icensored,
                2])/2, as.vector(x[icensored]), pch = 4,
                cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100)
    }
    invisible()
}
