#' Title
#'
#' @param x 
#' @param cdpoints.out 
#' @param distribution 
#' @param log.of.data 
#' @param how.show.fhat 
#' @param shape 
#' @param point.cex 
#' @param pch 
#' @param point.pch 
#' @param xlim 
#' @param col.points 
#' @param plot.vert.line 
#' @param cex.axis 
#' @param cex.lab 
#' @param ... 
#'
#' @return NULL
#' @export
plot.nonparametric.estimate <-
function (x, 
          cdpoints.out,
          distribution, 
          log.of.data,
          how.show.fhat, 
          shape, 
          point.cex, 
          pch, 
          point.pch, 
          xlim,
          col.points = 1,
          plot.vert.line = T, 
          cex.axis = 1.1, 
          cex.lab = 1.1,...)
{
    switch(how.show.fhat, sf = , step.fun = {
        at.point <- x$p == x$q
        over.interval <- !(x$p == x$q)
        x$q[x$q > 10^20] <- 1.1 * xlim[2]
        if (is.logdist(distribution)) x$p[x$p <= 0] <- x$p[2]/2
        if (any(at.point)) points.default(pp.data(x$p[at.point],log.of.data), 
                                          pp.quant(x$prob[at.point],distribution, shape = shape), 
                                          pch = point.pch, 
                                          cex = (point.cex * GetSMRDDefault("SMRD.point.size"))/100, 
                                          col = col.points)
        if (any(over.interval)) segments(pp.data(x$p[over.interval],log.of.data), 
                                         pp.quant(x$prob[over.interval],distribution, shape = shape),
                                         pp.data(x$q[over.interval],log.of.data), 
                                         pp.quant(x$prob[over.interval],distribution, shape = shape), 
                                         lty = 1, 
                                         lwd = 2, 
                                         col = col.points)
    }, points = , p = {
        if (is.logdist(distribution)) cdpoints.out$yplot[cdpoints.out$yplot <= 0] <- cdpoints.out$yplot[2]/2
        points.default(pp.data(cdpoints.out$yplot, log.of.data),
                       pp.quant(cdpoints.out$pplot, distribution, shape = shape),
                       cex = (point.cex * GetSMRDDefault("SMRD.point.size"))/100,
                       pch = pch, col = col.points)
    }, stop(paste("the how.show.fhat variable not recognized \n",
        "Must use step.fun (sf) or points (p)")))
}
