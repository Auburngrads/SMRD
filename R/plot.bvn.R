plot.bvn <-
function (x, muy, sdx, sdy, rho, xlim = NULL, ylim = NULL,
    fact = 2.5, number = 100, relative = F, add = T, ...)
{
    if (is.null(xlim))
        rx <- seq(x - fact * sdx, x + fact * sdx, length = number)
    else rx <- seq(xlim[1], xlim[2], length = number)
    if (is.null(ylim))
        ry <- seq(muy - fact * sdy, muy + fact * sdy, length = number)
    else ry <- seq(ylim[1], ylim[2], length = number)
    zmat <- exp(outer(rx, ry, dbvnl, x, muy, sdx, sdy, rho))
    if (relative)
        zmat <- zmat/max(zmat)
    contour(rx, ry, zmat, add = add, ...)
    invisible()
}
