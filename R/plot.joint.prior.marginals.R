#' @export
plot.joint.prior.marginals <-
function (x, logger = "xy", whichxy = c(1, 2), number.plot = 500,
    widthx = NULL, widthy = NULL,...)
{
    old.par <- par(mfcol = c(2, 2), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    plot.joint.prior(x, logger, whichxy, number.plot)
    plot.marginal.prior(x = x, logger = F, whichxy = whichxy[1],
        number.plot = number.plot, width = widthx)
    plot.marginal.prior(x = x, logger = F, whichxy = whichxy[2],
        number.plot = number.plot, width = widthy)
}
