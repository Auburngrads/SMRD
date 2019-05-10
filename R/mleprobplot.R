mleprobplot <-
function (data.ld,
          distribution,
          gamthr = 0,
          xlab = NULL,
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          time.range = c(NA, NA),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          my.title = NULL,
          cex = 1,
          cex.labs = 1.1,
          cex.axis = 1.1,
          sub.title = NULL,
          grids = F,
          linear.axes = F,
          slope.axis = F,
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          trunc.correct = T,
          add = F,
          cex.points = 1.2,
          theta.start = c(NA, NA),
          parameter.fixed = NULL,
          plot.censored.ticks = F,
          pch = 16,
          print.parameters = T,
          band.type = "p",
          debug1 = F,
          lwd.ci = 3,
          lwd.fhat = 2,
          band.method = GetSMRDDefault("SMRD.ConfidenceBandMethod"),
          mono.tran = T,
          title.line.adj,
          param.loc = "bottomright",...)
{
    if (missing(title.line.adj)) { title.line.adj = -3 }
  
    if (is.null(xlab)) {
      
        xlab <- SMRD2:::get.time.units(data.ld)
        if (!is.null(gamthr) && gamthr != 0)
            xlab <- paste(xlab, "-", gamthr)
    }
  
    cdfest.out <- SMRD2:::cdfest(data.ld, gamthr = gamthr)
    cdpoints.out <- SMRD2:::cdpoints(cdfest.out)
    
    if(SMRD2:::is.logdist(distribution)) {
      
       if(any(SMRD2:::Response(data.ld) <= 0)) stop("log-distribution specified but nonpositive response(s) in your life data object.")
      
    }
    
    mlest.out <- SMRD2:::mlest(data.ld, 
                               distribution, 
                               theta.start = theta.start,
                               parameter.fixed = parameter.fixed, 
                               gamthr = gamthr, ...)
    
    trunc.correct <- (!is.null(cdfest.out$left.trun.cond) || !is.null(cdfest.out$right.trun.cond)) && trunc.correct
    
    if(trunc.correct) {
      
       cdpoints.out <- SMRD2:::truncadj(cdpoints.out, mlest.out,debug1= debug1)
       
    }
    trunc.correct.string <- attr(cdpoints.out, "trunc.correct.string")
    ybandrange <- NULL
    xtvna <- is.na(time.range)
    if (any(xtvna)) time.range[xtvna] <- range(cdpoints.out$yplot)[xtvna]
    
    if (SMRD2:::map.SMRDDebugLevel() >= 4) {
      
        cat("Band method is", band.method, "band type is", band.type,
            "Confidence level=", conf.level, "\n")
      
    }
    switch(band.method, logit.cdf.method = {
        bands <- SMRD2:::get.parametric.bands(mlest.out, conf.level = conf.level,
            xlim = time.range, mono.tran = mono.tran)
        xbandrange <- NULL
        ybandrange <- range(SMRD2:::strip.na(bands$lower), SMRD2:::strip.na(bands$upper))
    }, zhat.cdf.method = {
        bands <- SMRD2:::get.parametric.bands.zhat(mlest.out, conf.level = conf.level,
            xlim = time.range, mono.tran = mono.tran)
        xbandrange <- NULL
        ybandrange <- range(SMRD2:::strip.na(bands$lower), SMRD2:::strip.na(bands$upper))
    }, log.quantile.method = {
        bands <- SMRD2:::get.parametric.bands.quant(mlest.out, conf.level = conf.level,
            xlim = time.range)
        ybandrange <- NULL
        xbandrange <- range(SMRD2:::strip.na(bands$lower), SMRD2:::strip.na(bands$upper))
    }, {
        stop(paste(band.method, "Not recognized"))
    })
    
    if (any(is.na(ybandrange))) ybandrange <- NULL
    xrna <- is.na(xlim) 
    if (any(xrna)) xlim[xrna] <- range(xbandrange, time.range, cdpoints.out$yplot)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(cdpoints.out$pplot, ybandrange)[yrna]
    
    `if`(band.type != "n",
         conf.int.title <- paste("and Pointwise", paste(floor(conf.level * 100 + 0.01), "%", sep = ""), "Confidence Intervals"),
         conf.int.title <- "")
    
    if (is.null(my.title)) {
      
        my.title <- paste(SMRD2:::get.data.title(data.ld), 
                          "\n", "with",
                          distribution, 
                          "ML Estimate ", 
                          conf.int.title, 
                          trunc.correct.string)
        
    }
    
    if(!missing(gamthr) && is.null(sub.title)) {
      
       `if`(SMRD2:::is.postsctiptok(),
            { sub.title <- paste("~f13~.g~f3~. = ", format(gamthr),"~", sep = "") ; cex <- 1.2 },
              sub.title <- paste("shift gamma = ", format(gamthr)))
        
     } else {
    
       if (is.null(sub.title)) sub.title <- ""
        
     }
    
    if(!add) {
      
       log.of.data <- SMRD2:::probplot.setup(distribution, 
                                     range(xlim),
                                     ylim, 
                                     my.title = my.title, 
                                     sub.title = sub.title,
                                     cex = cex, 
                                     cex.labs = cex.labs, 
                                     grids = grids, 
                                     linear.axes = linear.axes,
                                     title.option = title.option, 
                                     slope.axis = slope.axis,
                                     ylab = ylab, 
                                     xlab = xlab, 
                                     title.line.adj = title.line.adj,...)
        
     } else {
    
       log.of.data <- SMRD2:::get.prob.scales(distribution, 
                                      shape = NULL,
                                      prob.range = ylim)$logger
       
     }
    
     SMRD2:::plot.nonparametric.estimate(x = cdfest.out, 
                                         cdpoints.out, 
                                         distribution,
                                         log.of.data, 
                                         how.show.fhat = "points", 
                                         shape = NULL,
                                         point.cex = cex.points, 
                                         pch = pch, 
                                         point.pch = pch, 
                                         xlim,
                                         cex.axis = cex.axis, 
                                         cex.lab = cex.labs)
     
    bands.over <- bands$bands.over
    times <- bands$times
    fhat <- bands$fhat
    
    if(!is.null(fhat)) {
      
       lines(SMRD2:::pp.data(times[bands.over], log.of.data), 
             SMRD2:::pp.quant(fhat, distribution)[bands.over], 
             col = "black", lwd = lwd.fhat)
      
    }
    
    switch(band.method, 
           zhat.cdf.method = , logit.cdf.method = {
             
           if(band.type != "n" && !is.null(ybandrange)) {
             
              SMRD2:::plot.bands(bands, 
                                 distribution = distribution, 
                                 log.of.data = log.of.data,
                                 lwd.ci = lwd.ci, 
                                 lwd.fhat = lwd.fhat)
             
           }
             
        }, log.quantile.method = {
          
           if(band.type != "n" && !is.null(xbandrange)) {
             
              SMRD2:::plot.bands.quantile(bands, 
                                          distribution = distribution,
                                          log.of.data = log.of.data, 
                                          lwd.ci = lwd.ci, 
                                          lwd.fhat = lwd.fhat,
                                          mono.tran = mono.tran)
             
           }
          
        }, { stop(paste(band.method, "not recognized"))
          
        })
    
    SMRD2:::f.plot.censored.ticks(data.ld, log.of.data, plot.censored.ticks)
    SMRD2:::f.print.parameters(mlest.out, print.parameters, param.loc = param.loc)
}
