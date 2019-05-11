npprobplot <-
function (data.ld,
          distribution,
          shape = NULL,
          xlab = NULL,
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          band.type = "Simultaneous",
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          a.limit = 0.001,
          b.limit = 0.999,
          interactive = T,
          my.title = NULL,
          sub.title = NULL,
          point.cex = 1.2,
          how.show.fhat = "points",
          how.show.interval = "step.fun",
          grids = 0,
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          trunc.correct = T,
          slope.axis = F,
          draw.line = F,
          linear.axes = F,
          add = F,
          pch = 16,
          plot.censored.ticks = F,
          point.pch = 18,
          debug1 = F,
          col.points = 1,
          gamthr = NULL,
          title.line.adj, ...)
{
  
    if (missing(title.line.adj)) {  title.line.adj = -2.5 }
  
    if (is.null(xlab)) {
      
        xlab <- get.time.units(data.ld)
        if (!is.null(gamthr)) xlab <- paste(xlab, "-", gamthr)
        
    }
  
    if (!is.null(gamthr)) Response(data.ld) <- Response(data.ld) - gamthr
    
    cdfest.out <- cdfest(data.ld)
    
    if (is.logdist(distribution)) {
      
        if (any(Response(data.ld) <= 0)) stop("log-distribution specified but nonpositive response(s) in your life data object.")
      
    }
    
    default.title <- get.data.title(data.ld)
    there.is.truncation <- F
    if (trunc.correct && (!is.null(cdfest.out$left.trun.cond) || !is.null(cdfest.out$right.trun.cond))) {
      
        there.is.truncation <- T
        mlest.out <- mlest(data.ld, 
                           distribution, 
                           gamthr = 0,...)
        trunc.est.ok <- mlest.out$iervcv == 0
        
    }
    
    if (trunc.correct && there.is.truncation && trunc.est.ok) {
      
        cdfest.out <- trunc.adj.cdfest.out(cdfest.out, 
                                           mlest.out,
                                           debug1 = debug1)
        trunc.correct.string <- "\nwith truncation-corrected nonparametric estimate"
  
      } else {
          
        trunc.correct.string <- ""
    
      }
    
    cdpoints.out <- cdpoints(cdfest.out)
    
    if (band.type == "none" || is.null(cdfest.out$sd)) {
        band.type <- "none"
        ybandrange <- NULL
  
      } else {
    
        bands <- get.npbands(cdfest.out = cdfest.out, 
                             band.type = band.type,
                             conf.level = conf.level, 
                             how.show.interval = how.show.interval,
                             a.limit = a.limit, 
                             b.limit = b.limit)
        
        ybandrange <- c(strip.na(bands$lower), 
                        strip.na(bands$upper))
        ybandrange <- range(ybandrange[ybandrange > 0 & ybandrange < 1])
        
        }
    
    xvalues <- c(cdfest.out$p, cdfest.out$q)
    finite.xvalues <- xvalues[xvalues > -10^20 & xvalues < 10^20]
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(finite.xvalues)[xrna]
    xlim[1] <- xlim[1] + 1e-10
    xlim[2] <- xlim[2] - 1e-10
    
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(cdpoints.out$pplot, ybandrange)[yrna]
    ylim[1] <- ylim[1] + 1e-10
    ylim[2] <- ylim[2] - 1e-10
    
    if(is.null(my.title)) {
      
       `if`(band.type != "none",
            my.title <- paste(default.title, "\n", "with Nonparametric ", 
                bands$band.type, percent.conf.level(conf.level), 
                "Confidence Bands", trunc.correct.string),
            my.title <- paste(default.title, "\n", "Nonparametric CDF Estimate", 
                trunc.correct.string))
    }
 
    if(!add) {
      
       log.of.data <- probplot.setup(distribution = distribution,
                                     xlim = xlim, 
                                     ylim = ylim, 
                                     xlab = xlab,
                                     my.title = my.title, 
                                     sub.title = sub.title, 
                                     shape = shape,
                                     grids = grids, 
                                     title.option = title.option, 
                                     slope.axis = slope.axis,
                                     draw.line = draw.line, 
                                     linear.axes = linear.axes,
                                     ylab = ylab, 
                                     title.line.adj = title.line.adj, ...)
     } else {
    
       log.of.data <- get.prob.scales(distribution = distribution,
                                      shape = NULL, 
                                      prob.range = ylim)$logger
       
     }
    
    plot.nonparametric.estimate(cdfest.out, 
                                cdpoints.out, 
                                distribution,
                                log.of.data, 
                                how.show.fhat, 
                                shape, 
                                point.cex, 
                                pch, 
                                point.pch,
                                xlim, 
                                col.points = col.points)
    
    if(grids) {
      
       lwd.ci <- 3
       lwd.fhat <- 3
        
     } else {
    
       lwd.ci <- 3
       lwd.fhat <- 2
       
    }
    if (band.type != "none") {
      
        plot.bands(bands, 
                   distribution = distribution, 
                   shape = shape,
                   log.of.data = log.of.data, 
                   lwd.fhat = lwd.fhat, 
                   lwd.ci = lwd.ci,
                   cex.point = point.cex)
      
    }
    
    f.plot.censored.ticks(data.ld, log.of.data, plot.censored.ticks)
    invisible()
    cdfest.out$band.type <- band.type
    invisible(cdfest.out)
    
}