mfmc.mleprobplot <-
function (data.ld, 
          distribution, 
          xlab = SMRD:::get.time.units(data.ld),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), 
          xlim = c(NA,NA), 
          ylim = c(NA,NA), 
          time.range = c(NA, NA), 
          time.vector.length = 20,
          dump = 1, 
          grids = F, 
          cex = 1.2, 
          linear.axes = F,
          plot.censored.ticks = F, 
          ci.list = NULL,
          cex.points = 1.2, 
          shape = NULL, 
          do.legend = "On plot", 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          band.type = "pointwise", 
          col.ci = 4, 
          lwd.ci = 2, 
          show.individual = T,
          slope.axis = F, 
          cexlab = 1.5, ...)
{
    if (length(distribution) > 1)
        stop("Distribution should be a single name; use distribution.vec for multiple distributions")
    
    band.type <- SMRD:::generic.band.type(band.type)
    data.mfld <- SMRD:::ld.to.mfld(data.ld)
    distribution.vec = rep(distribution, length = length(data.mfld))
    failure.modes = names(data.mfld)
    col.fhat.vec = 1:length(data.mfld)
    lty = rep(1, length(data.mfld))
    lwd = rep(1, length(lty))
    
    if (length(distribution.vec) != length(data.mfld)) {
      
        warning(paste("distribution.vec has incorrect\nlength",
                      paste(distribution.vec, collapse = ","), 
                      "number of\nmodes = ", 
                      length(data.mfld)))
        distribution.vec <- rep(distribution, 
                                length = length(data.mfld))
    }
    
    switch(casefold(band.type), 
           n = , none      = { doing.bands <- F }, 
           p = , pointwise = { doing.bands <- T }, 
                             { warning("Band type", band.type, "not recognized")
                               doing.bands <- F })
    
    combined.lwd <- 3
    combined.lty <- 1
    combined.color <- 'purple'
    lwd[lty == 2] <- 4
    xtvna <- is.na(time.range)
    if (any(xtvna)) time.range[xtvna] <- SMRD:::get.time.range(data.ld, 
                                                        distribution = distribution)[xtvna]
    
    ylim.data <- range(SMRD:::strip.na(SMRD:::cdpoints(SMRD:::cdfest(data.ld))$pplot))
    
    `if`(SMRD:::is.logdist(distribution),
         time.vector <- SMRD:::logseq(time.range[1], 
                               time.range[2],
                               length = time.vector.length),
         time.vector <-    seq(time.range[1], 
                               time.range[2], 
                               length = time.vector.length))
    if (show.individual) {
      
     multiple.mlest.out <- 
      mfmi.mleprobplot(data.ld, 
                       distribution = distribution,
                       distribution.vec = distribution.vec, 
                       xlim = xlim,
                       xlab = xlab, 
                       ylim = ylim, 
                       ylab = ylab, 
                       grids = grids,
                       linear.axes = linear.axes, 
                       title.option = title.option,
                       ci.list = ci.list, 
                       plot.censored.ticks = F,
                       time.range = range(time.vector), 
                       plot.frame = T,
                       ylim.data = ylim.data, 
                       cex = cex, 
                       plot.np = F,
                       slope.axis = slope.axis, 
                       plotem = rep(T, length = length(data.mfld)),
                       do.legend = "Suppress")
    
     } else {
      
       multiple.mlest.out <- 
         mfmi.mleprobplot(data.ld, 
                          distribution = distribution,
                          distribution.vec = distribution.vec, 
                          xlim = xlim,
                          xlab = xlab, 
                          ylim = ylim, 
                          ylab = ylab, 
                          grids = grids,
                          linear.axes = linear.axes, 
                          ci.list = ci.list, 
                          plot.censored.ticks = F,
                          time.range = range(time.vector), 
                          plot.frame = F,
                          ylim.data = ylim.data, 
                          cex = cex, 
                          plot.np = F,
                          plotem = rep(F, length = length(data.mfld)), 
                          do.legend = "Suppress")
       
           probplot.setup(distribution, 
                          xlim = range(time.vector),
                          ylim = ylim.data, 
                          cex = cex,
                          cexlab = cexlab, 
                          grids = grids, 
                          linear.axes = linear.axes,
                          slope.axis = slope.axis,
                          ylab = ylab, 
                          xlab = xlab)
        } 
    
    if (length(data.mfld) < 2)
        stop(paste("Only one failure mode", 
                   names(data.mfld),
                   "in a requested failure mode analysis"))
    
    `if`(doing.bands,
         band.type.send <- band.type,
         band.type.send <- "none")
    
    mfmc.probs.out <- 
      SMRD:::failure.probabilities.mfmc(x = multiple.mlest.out,
                                 time.vector = time.vector, 
                                 band.type = band.type.send, 
                                 conf.level = conf.level)
    
    log.of.data <- is.logdist(distribution)
    
    lines(pp.data(mfmc.probs.out[, 1], log.of.data), 
          pp.quant(mfmc.probs.out[, "Fhat"], 
                   distribution, 
                   shape), 
          lwd = combined.lwd, 
          col = combined.color)
    
    if (doing.bands) {
      
        lines(pp.data(mfmc.probs.out[, 1], log.of.data),
              pp.quant(mfmc.probs.out[, 4], distribution, shape), 
              lwd = lwd.ci, 
              col = col.ci,
              lty = 3)
        lines(pp.data(mfmc.probs.out[, 1], log.of.data), 
              pp.quant(mfmc.probs.out[, 5], distribution, shape), 
              lwd = lwd.ci, 
              col = col.ci,
              lty = 3)
    }
    
    cdpoints.out <- cdpoints(cdfest(data.ld))
    
    points.default(pp.data(cdpoints.out$yplot, log.of.data),
                   quant(cdpoints.out$pplot, distribution), 
                   cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100, 
                   pch = 16)
    
    plotted <- rep(T, length(multiple.mlest.out))
    
    if ((do.legend == "On plot" || do.legend == "New page") &&
        any(plotted) && show.individual) {
        if (do.legend == "New page")
            plot(c(0, 0), 
                 c(1, 1), 
                 xlab = "", 
                 ylab = "", 
                 type = "n",
                 xaxt = "n", 
                 yaxt = "n")
      
        legend(x.loc(0.002), 
               y.loc(0.996), 
               c(names(data.mfld)[plotted],"Combined"), 
               cex = 1.05, 
               bty = "n", 
               col = c(col.fhat.vec[plotted],combined.color), 
               lty = c(lty, combined.lty), 
               lwd = c(lwd, combined.lwd),
               y.intersp = 0.675)
    }
    invisible(multiple.mlest.out)
}
