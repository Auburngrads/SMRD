mfmi.mleprobplot <-
function (data.ld, 
          distribution, 
          distribution.vec = rep(distribution, length = length(data.mfld)), 
          the.failure.modes = names(data.mfld),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          xlab = get.time.units(data.ld),
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          time.range = c(NA, NA), 
          dump = 1, 
          grids = F, 
          cex = 1.2, 
          linear.axes = F, 
          pch = (1:(length(the.failure.modes) + 1))[-2], 
          lty = rep(1, length(the.failure.modes)), 
          ci.list = NULL, 
          lwd = rep(1, length(the.failure.modes)), 
          slope.axis = F, 
          plot.censored.ticks = F, 
          trunc.correct = T, 
          col.fhat.vec = (1:(length(the.failure.modes) + length(col.ci)))[-col.ci], 
          col.ci = 4, 
          shape = NULL, 
          do.legend = NULL, ...) 
{
    data.mfld <- ld.to.mfld(data.ld)
    
    if(is.null(do.legend)) do.legend = 'On plot'
    
    if (length(distribution.vec) != length(data.mfld)) {
        warning(paste("distribution.vec has incorrect\nlength",
                      paste(distribution.vec, collapse = ","), 
                      "number of\nmodes=",
                      length(data.mfld)))
      
    distribution.vec <- rep(distribution, length = length(data.mfld))
    }
    
    parametric.list <- 
      multiple.mleprobplot(data.mfld, 
                           data.ld.name = deparse(substitute(data.ld)), 
                           distribution = distribution, 
                           distribution.vec = distribution.vec, 
                           xlab = xlab, 
                           ylab = ylab, 
                           conf.level = conf.level, 
                           xlim = xlim, 
                           ylim = ylim, 
                           time.range = time.range, 
                           dump = dump, 
                           grids = grids, 
                           cex = cex, 
                           linear.axes = linear.axes, 
                           pch = pch, 
                           lty = lty, 
                           ci.list = ci.list, 
                           lwd = lwd, 
                           col.fhat.vec = col.fhat.vec, 
                           col.ci = col.ci, 
                           shape = shape, 
                           trunc.correct = trunc.correct, 
                           plot.censored.ticks = plot.censored.ticks, 
                           slope.axis = slope.axis, 
                           do.legend = do.legend, 
                           do.list = the.failure.modes, ...)
    
    oldClass(parametric.list) <- c("mfm.multiple.mlest.out",
                                   "multiple.mlest.out")
    
    MysetOldClass(attr(parametric.list, "class"))
    return(parametric.list)
}
