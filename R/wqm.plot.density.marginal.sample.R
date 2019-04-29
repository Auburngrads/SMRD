wqm.plot.density.marginal.sample <-
function (marginal.sample, xlab, xlim = c(NA, NA), ylim = c(NA, 
    NA), ylab = "", my.title = NULL, cex = 1) 
{
    marginal.trim.range <- range(trim.vector(marginal.sample, 
        trim = 0.005))
    x.width <- (4 * (marginal.trim.range[2] - marginal.trim.range[1]))/(logb(length(marginal.sample), 
        base = 2) + 1)
    density.out <- density(marginal.sample, width = x.width, 
        from = marginal.trim.range[1], to = marginal.trim.range[2])
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(density.out$x)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(density.out$y)[yrna]
    plot.paper(xlim, ylim, xlab = xlab, ylab = ylab, y.axis = "blank", 
        grids = F, cex = cex, cex.labs = cex, cex.tic.lab = cex)
    lines(density.out, lwd = 2)
    abline(h = 0)
}
