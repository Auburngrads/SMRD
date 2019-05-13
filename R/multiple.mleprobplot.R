#' Title
#'
#' @param data.list 
#' @param data.ld.name 
#' @param distribution 
#' @param distribution.vec 
#' @param xlab 
#' @param ylab 
#' @param conf.level 
#' @param check.level 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param dump 
#' @param grids 
#' @param my.title 
#' @param cex 
#' @param linear.axes 
#' @param slope.axis 
#' @param pch 
#' @param lty 
#' @param ci.list 
#' @param lwd 
#' @param trunc.correct 
#' @param col.fhat.vec 
#' @param col.ci 
#' @param shape 
#' @param do.legend 
#' @param add.title 
#' @param do.list 
#' @param plotem 
#' @param plot.censored.ticks 
#' @param ylim.data 
#' @param plot.np 
#' @param plot.frame 
#' @param debug1 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1,
#'                                 failure.mode.column = 2,
#'                                 censor.column = 3, 
#'                                 time.units = "Kilometers")
#' summary(ShockAbsorber.ld)
#' event.plot(ShockAbsorber.ld)
#' 
#' # Split out by failure mode
#' 
#' mleprobplot(ShockAbsorber.ld, 
#'             distribution = "Weibull")
#' 
#' mfmi.mleprobplot(ShockAbsorber.ld, 
#'                  distribution = "Weibull")
#' 
#' mfmc.mleprobplot(ShockAbsorber.ld, 
#'                  distribution = "Weibull")
#'                  
#' ShockAbsorber.mfld <- mfm.to.ld(ShockAbsorber.ld)
#' 
#' multiple.mleprobplot(ShockAbsorber.mfld,
#'                      data.ld.name="xx",
#'                      xlab="yy",
#'                      distribution="Weibull")
#' 
#' mleprobplot(ShockAbsorber.Mode1.ld, 
#'             distribution = "Weibull")
#' 
#' mleprobplot(ShockAbsorber.Mode2.ld,
#'             distribution = "Weibull")
#' 
#' get.time.vector(ShockAbsorber.Mode2.ld)
#' }
multiple.mleprobplot <-
function (data.list, 
          data.ld.name, 
          distribution, 
          distribution.vec = rep(distribution, length = length(data.list)), 
          xlab, 
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          check.level = SMRDOptions("SMRD.DataCheck"),
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          time.range = c(NA, NA), 
          dump = 1,
          grids = F, 
          my.title = NULL, 
          cex = 1.2, 
          linear.axes = F, 
          slope.axis = F, 
          pch = (1:(length(do.list) + 1))[-2], 
          lty = rep(1, length(do.list)), 
          ci.list = NULL, 
          lwd = rep(1, length(do.list)),
          trunc.correct = T, 
          col.fhat.vec = (1:(length(do.list) + length(col.ci)))[-col.ci],
          col.ci = 4, 
          shape = NULL, 
          do.legend = NULL, 
          add.title = NULL,
          do.list = names(data.list), 
          plotem = rep(T, length(do.list)),
          plot.censored.ticks = F, 
          ylim.data = NULL, 
          plot.np = T,
          plot.frame = T,
          debug1 = F,...)
{

    bands.list <- list()
    parametric.list <- list()
    number.good.parametric <- 0
    nonparametric.list <- list()
    plotted <- rep(F, length(do.list))
    time.range.all <- NULL
    if(is.null(do.legend)) do.legend <- "On plot"
    
    for (i in 1:length(do.list)) {
          data.name <- do.list[i]
          data.subset.ld <- data.list[[do.list[i]]]
          parametric.list[[data.name]] <- NA
          if (!good.data(data.subset.ld, check.level = check.level)) {
            
            cat(paste("\nSkipping", 
                      data.name, 
                      "because too few failures\n"))
            next
        }
        
        cdfest.out <- cdfest(data.subset.ld)
        
        if (length(cdfest.out$q) <= 0) {
            cat(paste("\nSkipping", 
                      data.name, 
                      "because cannot do cdfest\n"))
            next
        }
        plotted[i] <- T
        cdpoints.out <- cdpoints(cdfest.out)
        
        trunc.correct <- (!is.null(cdfest.out$left.trun.cond) ||
            !is.null(cdfest.out$right.trun.cond)) && trunc.correct
        
        mlest.out <- mlest(data.subset.ld, distribution.vec[i],...)
        
        number.good.parametric <- number.good.parametric + 1
        if (trunc.correct) {
            cdpoints.out <- truncadj(cdpoints.out, 
                                     mlest.out,
                                     debug1= debug1)
            
            trunc.correct.string <- "\nWith Truncation-Corrected Nonparametric Estimate"
            
        } else {
          
            trunc.correct.string <- ""
        }
        
        nonparametric.list[[data.name]] <- cdpoints.out
        
        if (mlest.out$iervcv > 0) {
            if (is.DebugOn()) {
                file.name <- paste("ProblemData", floor(runif(1) *
                  1e+07), ".ld", sep = "")
                assign(envir = .frame0,  inherits = TRUE,file.name, data.subset.ld)
                cat("\nCheck stored data in", file.name, "\n")
                warning(paste("Problem with convergence with",
                              data.name, 
                              "iervcv = ", 
                              mlest.out$iervcv, 
                              "\nsaved in",
                              file.name))
            }
        }
        mlest.out$title <- paste(mlest.out$title, data.name)
        if (map.SMRDDebugLevel() >= 4) {
            cat("dev in multiple.mleprobplot: data.name,names(data.list),ci.list,names(data.list)[ci.list]\n")
            print(data.name)
            print(names(data.list)[ci.list])
            print(names(data.list))
            print(ci.list)
        }
        `if`(is.onlist(data.name, names(data.list)[ci.list]),
             conf.level.send <- conf.level,
             conf.level.send <- 0)
        
        prob.vec <- c(0.65 * min(cdpoints.out$pplot), 
                      0.99 * max(cdpoints.out$pplot + 0.01))
        if (debug1) {
            cat("\n------------------------------------\n")
            print(i)
            print(cdpoints.out$yplot)
            print(cdpoints.out$pplot)
            cat("\n------------------------------------\n")
            cat("\n------------------------------------\n")
        }
        the.quantiles <- quantiles(mlest.out, 
                                   print = F, 
                                   prob.vec = prob.vec)[,"Quanhat"]
        
        time.range.now <- time.range
        
        xtvna <- is.na(time.range.now)
        if (any(xtvna)) time.range.now[xtvna] <- range(the.quantiles)[xtvna]
        
        time.range.all <- range(time.range.all, time.range.now)
        bands <- get.parametric.bands.zhat(mlest.out, 
                                           conf.level = conf.level.send,
                                           xlim = time.range.now)
        mlest.out$bands <- bands
        parametric.list[[data.name]] <- mlest.out
        bands.list[[data.name]] <- bands
        ylim.data <- range(ylim.data, 
                           cdpoints.out$pplot,
                           bands$fhat, 
                           bands$lower, 
                           bands$upper)
    }
    if (number.good.parametric == 0) {
        warning(paste("No estimable data sets in", data.ld.name))
        return(NULL)
    }
    xlim.new <- time.range.all
    xrna <- is.na(xlim)
    if (any(xrna)) {
        for (i in 1:length(do.list)) {
            data.subset.ld <- data.list[[do.list[i]]]
            if (good.data(data.subset.ld))
                xlim.new <- range(xlim.new, 
                                  get.time.range(data.subset.ld, distribution))
        }
    }
    xlim[xrna] <- xlim.new[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- ylim.data[yrna]
    
    if (plot.frame) {
        log.of.data <- probplot.setup(distribution, 
                                      xlim,
                                      ylim, 
                                      xlab = xlab, 
                                      ylab = ylab,
                                      grids = grids, 
                                      linear.axes = linear.axes, 
                                      slope.axis = slope.axis,
                                      cex = cex)
        for (i in 1:length(do.list)) {
            data.name <- do.list[i]
            if (is.null(nonparametric.list[[data.name]]))
                next
            data.subset.ld <- data.list[[data.name]]
            bands <- bands.list[[data.name]]
            times <- bands$times
            cdpoints.out <- nonparametric.list[[data.name]]
            if (plotem[i]) {
                if (plot.np)
                  points.default(pp.data(cdpoints.out$yplot,log.of.data), 
                                 quant(cdpoints.out$pplot, distribution),
                                 pch = pch[i]%%19, 
                                 col = col.fhat.vec[i],
                                 cex = (cex * GetSMRDDefault("SMRD.point.size"))/100)
              
                  lines(pp.data(times, log.of.data), 
                        pp.quant(bands$fhat,distribution, shape), 
                        col = col.fhat.vec[i],
                        lty = lty[i], 
                        lwd = 2)
                if (lty[i] == 2)
                  lines(pp.data(times, log.of.data), 
                        pp.quant(bands$fhat,distribution, shape), 
                        col = col.fhat.vec[i],
                        lty = lty[i], 
                        lwd = 3)
                if (!is.null(bands$lower)) {
                  lines(pp.data(times, log.of.data), 
                        pp.quant(bands$lower,distribution, shape), 
                        col = col.ci, 
                        lty = 3,
                        lwd = 2)
                  lines(pp.data(times, log.of.data), 
                        pp.quant(bands$upper,distribution, shape), 
                        col = col.ci, 
                        lty = 3,
                        lwd = 2)
                }
                f.plot.censored.ticks(data.subset.ld, 
                                      log.of.data,
                                      plot.censored.ticks, 
                                      col = col.fhat.vec[i])
            }
        }
        plotted <- plotted & plotem
        do.list[plotted] <- switch.units(do.list[plotted])
        
        if (do.legend == "On plot" && any(plotted)) {
          
                legend(x.loc(0.003), 
                       y.loc(0.994), 
                       legend = parse(text = do.list[plotted]),
                       cex = 1, 
                       bty = "n", 
                       col = col.fhat.vec[plotted],
                       lty = lty[plotted], 
                       pch = pch[plotted]%%19, 
                       y.intersp = .65)
        }
        
      if (do.legend == "New page" && any(plotted)) {
        
            plot(x = c(0, 0), 
                 y = c(1, 1), 
                 xlab = "", 
                 ylab = "", 
                 type = "n",
                 xaxt = "n",
                 yaxt = "n")
            legend(x.loc(0.003), 
                   y.loc(0.994), 
                   do.list[plotted],
                   cex = 1, 
                   bty = "n", 
                   col = col.fhat.vec[plotted],
                   lty = lty[plotted], 
                   pch = pch[plotted]%%19)
            
            if (do.legend == "New file") dev.off()
      
        }
        
    }
    invisible()
    oldClass(parametric.list) <- "multiple.mlest.out"
    attr(parametric.list, "data.ld") <- attr(data.list, "data.ld")
    return(parametric.list)
}
