#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param gamthr 
#' @param stresses 
#' @param group.var 
#' @param xlab 
#' @param ylab 
#' @param conf.level 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param dump 
#' @param grids 
#' @param my.title 
#' @param cex 
#' @param linear.axes 
#' @param title.option 
#' @param pch 
#' @param lty 
#' @param ci.list 
#' @param lwd 
#' @param plot.censored.ticks 
#' @param trunc.correct 
#' @param col.fhat.vec 
#' @param col.ci 
#' @param shape 
#' @param do.legend 
#' @param stresses.limit 
#' @param plotem 
#' @param check.level 
#' @param title.line.adj 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' DeviceA.ld <- frame.to.ld(devicea, 
#'                           data.title = "Device-A ALT Results",
#'                           response.column = 1,
#'                           time.units = "Hours",
#'                           censor.column = 2,
#'                           case.weight.column = 3,
#'                           x.columns = 4, 
#'                           xlab = "Degrees C")
#' 
#' print(DeviceA.ld)
#' summary(DeviceA.ld)
#' 
#' censored.data.plot(DeviceA.ld)
#' 
#' censored.data.plot(DeviceA.ld, 
#'                    y.axis ="log", 
#'                    x.axis = "Arrhenius")
#' 
#' groupi.mleprobplot(DeviceA.ld, 
#'                    distribution = "Weibull")
#' 
#' four.groupi.mleprobplot(DeviceA.ld)
#' 
#' DeviceA.weib.groupi <- groupi.mleprobplot(DeviceA.ld,
#'                                           distribution = "Weibull")
#' 
#' print(DeviceA.weib.groupi)
#' summary(DeviceA.weib.groupi)
#' 
#' }
groupi.mleprobplot <-
function (data.ld, 
          distribution, 
          gamthr = 0, 
          stresses = get.x.markers(data.ld, group.var = group.var, do.order = T), 
          group.var = 1, 
          xlab = get.time.units(data.ld),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          time.range = c(NA,NA),
          dump = 1, 
          grids = F, 
          my.title = NULL, 
          cex = 1,
          linear.axes = F, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          pch = (1:(length(stresses) + 1))[-2], 
          lty = NULL, 
          ci.list = NULL, 
          lwd = rep(2, length(stresses)),
          plot.censored.ticks = F, 
          trunc.correct = T, 
          col.fhat.vec = (1:(length(stresses) + length(col.ci)))[-col.ci], 
          col.ci = 4, 
          shape = NULL,
          do.legend = "On plot", 
          stresses.limit = 18, 
          plotem = rep(T, length(stresses)),
          check.level = SMRDOptions("SMRD.DataCheck"),
          title.line.adj = -2,
          mar = c(4.5, 5.25, 3.5, 12.1),
          bty = `if`(grids, "o","L"),...)
{
  
    if (missing(title.line.adj)) {  title.line.adj = -3    }

    if (is.null(xlab)) {
      
    xlab <- get.time.units(data.ld)
    if (!is.null(gamthr) && gamthr != 0) xlab <- paste(xlab, "-", gamthr)
    
    }

    if (!is.null(gamthr)) Response(data.ld) <- Response(data.ld) - gamthr
    
    number.group.var <- length(group.var)
    
    if (is.null(lty)) {
      
    `if`(GetSMRDDefault("SMRD.solid.lines"),
         lty <- rep(1, length(stresses)),
         lty <- (1:(length(stresses) + 1))[-2])
    }
    
    if (is.null(my.title)) my.title <- paste(get.data.title(data.ld), 
                                             "\n", 
                                             "With Individual",
                                             distribution, 
                                             "Distribution ML Estimates")
    
    multiple.ld <- factor.ld.to.multiple.ld(data.ld, 
                                            group.var = group.var,
                                            stresses = stresses)
    plot.frame <- T
    
    if (length(plotem) > stresses.limit) {
        warning(paste("\n\nThere are", 
                      length(stresses),
                      "different explanatory variable combinations.\n",
                      length(stresses), 
                      "plots have been requested. A probability plot will not be made.\n\n"))
      
    plotem <- rep(F, length(stresses))
    plot.frame <- F
    
    } else {
      
        plotem.hold <- plotem
        plotem <- rep(F, length(stresses))
        plotem[plotem.hold] <- T
        
    }
    
      par(mar = mar, bty = bty)
      on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1,err = -1))
    
parametric.list <- 
  multiple.mleprobplot(multiple.ld, 
                       data.ld.name = deparse(substitute(data.ld)),
                       distribution = distribution, 
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
                       do.legend = do.legend, 
                       plot.frame = plot.frame, 
                       plotem = plotem,
                       title.line.adj = title.line.adj,
                       check.level = check.level,
                       mar = mar,
                       bty = bty,...)
    
log.of.data <- get.prob.scales(distribution, 
                               shape = NULL,
                               prob.range = c(0.1, 0.5))$logger
    
f.plot.censored.ticks(data.ld, 
                      log.of.data, 
                      plot.censored.ticks)
  
return(parametric.list)

}
