wqm.plot.density <-
function (x, 
          ylim = c(NA, NA), 
          xlim = c(NA, NA), 
          cex = 1,
          xlab = "x", 
          ylab = "density") 
{
    x.trim.range <- range(trim.vector(x, trim = 0.005))
    x.width <- (4 * (x.trim.range[2] - x.trim.range[1])) / (logb(length(x), 
        base = 2) + 1)
    
    density.out <- density(x,
                           bw = "SJ",
                           width = x.width, 
                           from = x.trim.range[1],
                           to = x.trim.range[2])
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(density.out$x)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(density.out$y)[yrna]
    
    plot.paper(xlim, 
               ylim, 
               xlab = xlab, 
               ylab = ylab, 
               y.axis = "blank", 
               grids = F, 
               cex = cex)
    
    lines(density.out, lwd = 2)
    
    invisible()
    
}
