#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param distribution.vec 
#' @param the.failure.modes 
#' @param ylab 
#' @param conf.level 
#' @param xlab 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param dump 
#' @param grids 
#' @param cex 
#' @param linear.axes 
#' @param pch 
#' @param lty 
#' @param ci.list 
#' @param lwd 
#' @param slope.axis 
#' @param plot.censored.ticks 
#' @param trunc.correct 
#' @param col.fhat.vec 
#' @param col.ci 
#' @param shape 
#' @param do.legend 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' DeviceG.ld <- frame.to.ld(deviceg,
#'                           response.column = 1, 
#'                           failure.mode.column =  2)
#'                           
#' # Plots the individual failure modes separately
#' 
#' par(mfrow = c(1,2))
#' mfmi.mleprobplot(DeviceG.ld, distribution = "weibull") 
#' mfmi.mleprobplot(DeviceG.ld, distribution = "lognormal") 
#' par(mfrow = c(1,1))
#' 
#' # Plots both modes in one plot
#' 
#' mfmc.mleprobplot(DeviceG.ld, 
#'                  distribution = "Weibull") 
#' 
#' mfmc.mleprobplot(DeviceG.ld, 
#'                  distribution = "Weibull", 
#'                  band.type = "none")
#'                  
#' # different distributions for the different failure modes
#' 
#' mfmc.mleprobplot(DeviceG.ld, 
#'                  distribution = "Weibull", 
#'                  distribution.vec = c("Weibull","Lognormal"))
#' }

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
