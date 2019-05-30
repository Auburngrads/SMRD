resid.vs.explan.multiple <-
function (groupm.list, 
          original.par = T, 
          cex = 1, 
          my.title = NULL, 
          grids = F, 
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          x.axis = "linear",
          x.to.plot = NULL, 
          cex.points = 1, ...) 
{
    the.groupm.object <- extract.results(groupm.list)
    
    `if`(!is.null(the.groupm.object$the.orig.data.ld),
         xmat <- xmat(the.groupm.object$the.orig.data.ld),
         xmat <- xmat(the.groupm.object$data.ld))
    
    xnames <- dimnames(xmat)[[2]]
    
    if (is.null(x.to.plot)) x.to.plot <- xnames
    
    on.list <- is.onlist(x.to.plot, xnames)
    
    if (any(!on.list)) stop(paste(x.to.plot[!on.list], "not in the x matrix"))
    
    for (i in 1:length(x.to.plot)) {
        
        resid.vs.explan(groupm.list = groupm.list, 
                        original.par = original.par,
                        cex = cex, 
                        my.title = my.title, 
                        grids = grids, 
                        xlim = xlim, 
                        ylim = ylim, 
                        x.axis = x.axis, 
                        x.to.plot = x.to.plot[i],
                        cex.points = cex.points,...)
        
    }
    invisible()
}
