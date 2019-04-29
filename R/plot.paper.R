plot.paper <-
function (x, 
          y, 
          x.axis = "linear", 
          y.axis = "linear",
          grids = FALSE, 
          my.title = "", 
          ylab = "", 
          xlab = "", 
          response.on.yaxis = T,
          cex = par()$cex, 
          cex.labs = par()$cex.lab, 
          cex.tic.lab = 1.1, 
          cex.title = par()$cex.main,
          mar = c(4.5, 5.25, 3.5, 2.1), 
          add = F, 
          hw.xaxis = NULL, 
          hw.yaxis = NULL,
          yaxis.line = NULL, 
          xaxis.labels = NULL, 
          at.xaxis.labels = 1:length(xaxis.labels),
          yaxis.labels = NULL, 
          at.yaxis.labels = 1:length(yaxis.labels),
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          title.line.adj = 0, ...)
{
    label.factor <- sqrt(prod(max(c(par("mfrow"), par("mfcol")))))
    
    `if`(prod(max(c(par("mfrow"), par("mfcol")))) > 1,
         label.line <- 2,
         label.line <- 0.75)
    
    par(mar = mar)
    if (missing(x)) x <- c(1, 100)
    if (missing(y)) y <- c(1, 100)
    
    GetAxesRange.out <- SMRD:::GetAxesRange("plot.paper", 
                                     x.axis, 
                                     xlim = x,
                                     xlab, 
                                     y.axis, 
                                     ylim = y, 
                                     ylab)
    x <- GetAxesRange.out$xlim
    y <- GetAxesRange.out$ylim
    
    if (response.on.yaxis) {
      
        x <- GetAxesRange.out$xlim
        y <- GetAxesRange.out$ylim
        
        } else {
          
        y <- GetAxesRange.out$xlim
        x <- GetAxesRange.out$ylim
        y.axis <- GetAxesRange.out$x.axis
        x.axis <- GetAxesRange.out$y.axis
        ylab <- GetAxesRange.out$xlab
        xlab <- GetAxesRange.out$ylab
    
        }
    
    g.x.axis <- SMRD:::generic.relationship.name(x.axis)
    
    if (is.null(hw.xaxis)) {
      
        `if`(g.x.axis == "reciprocal" || g.x.axis == "reciprocal3" ||
            (g.x.axis == "Box-Cox" && attr(x.axis, "the.power") < 0.5),
             log.like.x.axis <- T,
             log.like.x.axis <- F)
      
        if (g.x.axis == "log" || g.x.axis == "log10" || log.like.x.axis) {
          
            getxax.out <- SMRD:::logax(x[1], x[2], ...)
            
            } else {
              
            getxax.out <- SMRD:::linax(x[1], x[2], ...)
            
            switch(g.x.axis, 
                   
                   humidity = {
              
                if (as.numeric(getxax.out$ticlab[1]) <= 0) 
                    getxax.out$ticlab[1] <- " 1"
                if (as.numeric(getxax.out$ticlab[length(getxax.out$ticlab)]) >= 100) 
                    getxax.out$ticlab[length(getxax.out$ticlab)] <- "99"
                if (as.numeric(getxax.out$ticloc[1]) <= 0) 
                    getxax.out$ticloc[1] <- " 1"
                if (as.numeric(getxax.out$ticloc[length(getxax.out$ticloc)]) >= 100) 
                    getxax.out$ticloc[length(getxax.out$ticloc)] <- "99"
                
                }, logit = {
                  
                if (as.numeric(getxax.out$ticlab[1]) <= 0) 
                    getxax.out$ticlab[1] <- ".01"
                if (as.numeric(getxax.out$ticlab[length(getxax.out$ticlab)]) >= 1) 
                    getxax.out$ticlab[length(getxax.out$ticlab)] <- ".99"
                if (as.numeric(getxax.out$ticloc[1]) <= 0) 
                    getxax.out$ticloc[1] <- ".01"
                if (as.numeric(getxax.out$ticloc[length(getxax.out$ticloc)]) >= 1) 
                    getxax.out$ticloc[length(getxax.out$ticloc)] <- ".99"
            })
        }
  
    } else { getxax.out <- hw.xaxis }
    
    if (as.numeric(getxax.out$ticloc[1]) < as.numeric(getxax.out$ticlab[1])) {
      
        lower.ones <- as.numeric(getxax.out$ticloc) < as.numeric(getxax.out$ticlab[1])
        getxax.out$ticlab <- 
          c(getxax.out$ticloc[lower.ones], getxax.out$ticlab)
        
        }
    
    datax.tic.location <- as.numeric(getxax.out$ticloc)
    datax.tic.label.loc <- as.numeric(getxax.out$ticlab)
    g.y.axis <- SMRD:::generic.relationship.name(y.axis)
    
    if (is.null(hw.yaxis)) {
      
        `if`(g.y.axis == "reciprocal" || g.y.axis == "reciprocal3" ||
            (g.y.axis == "Box-Cox"    && attr(y.axis, "the.power") < 0.5),
            log.like.y.axis <- T,
            log.like.y.axis <- F)
      
        if (g.y.axis == "log" || g.y.axis == "log10" || log.like.y.axis) {
          
            getyax.out <- SMRD:::logax(y[1], y[2],...)
            if (is.null(yaxis.line)) yaxis.line <- 3.5
            
            } else {
              
            getyax.out <- SMRD:::linax(y[1], y[2],...)
            if (is.null(yaxis.line)) yaxis.line <- 3.5
        
            }
      
        switch(g.y.axis, 
               
               humidity = {
            if (as.numeric(getyax.out$ticlab[1]) <= 0) 
                getyax.out$ticlab[1] <- " 1"
            if (as.numeric(getyax.out$ticlab[length(getyax.out$ticlab)]) >= 100) 
                getyax.out$ticlab[length(getyax.out$ticlab)] <- "99"
            if (as.numeric(getyax.out$ticloc[1]) <= 0) 
                getyax.out$ticloc[1] <- " 1"
            if (as.numeric(getyax.out$ticloc[length(getyax.out$ticloc)]) >= 100) 
                getyax.out$ticloc[length(getyax.out$ticloc)] <- "99"
            
            }, logit = {
              
            if (as.numeric(getyax.out$ticlab[1]) <= 0) 
                getyax.out$ticlab[1] <- ".01"
            if (as.numeric(getyax.out$ticlab[length(getyax.out$ticlab)]) >= 1) 
                getyax.out$ticlab[length(getyax.out$ticlab)] <- ".99"
            if (as.numeric(getyax.out$ticloc[1]) <= 0) 
                getyax.out$ticloc[1] <- ".01"
            if (as.numeric(getyax.out$ticloc[length(getyax.out$ticloc)]) >= 1) 
                getyax.out$ticloc[length(getyax.out$ticloc)] <- ".99"
        })
      
      } else {
        
        getyax.out <- hw.yaxis
        if (is.null(yaxis.line)) yaxis.line <- 5
    
      }
    
    if (as.numeric(getyax.out$ticloc[1]) < as.numeric(getyax.out$ticlab[1])) {
      
        lower.ones <- 
          as.numeric(getyax.out$ticloc) < as.numeric(getyax.out$ticlab[1])
        getyax.out$ticlab <- 
          c(getyax.out$ticloc[lower.ones], getyax.out$ticlab)
    
    }
    
    datay.tic.location <- as.numeric(getyax.out$ticloc)
    datay.tic.label.loc <- as.numeric(getyax.out$ticlab)
    x.prange <- range(datax.tic.label.loc)
    y.prange <- range(datay.tic.label.loc)
    
    `if`(x.axis == "blank",
         x.axis.p <- "linear",
         x.axis.p <- x.axis)
    
    `if`(y.axis == "blank",
         y.axis.p <- "linear",
         y.axis.p <- y.axis)
    
    if (map.SMRDDebugLevel() >= 5)
        cat("In plot.paper x=", 
            paste(x.axis, x.axis.p, x.prange[1], x.prange[2], sep = ","), 
            "\n", 
            paste("y=", y.axis, y.axis.p, y.prange[1], y.prange[2], sep = ","), 
            "\n")
    
    plot(x = SMRD:::f.relationship(x.prange, x.axis.p), 
         y = SMRD:::f.relationship(y.prange, y.axis), 
         type = "n", 
         xaxt = "n", 
         yaxt = "n", 
         xlab = "",
         ylab = "", 
         cex = cex)
    
    if (title.option != "blank") {

            mtext(text = my.title, side = 3, cex = cex.title, line = title.line.adj)

    }
    
    title(xlab = xlab, cex.lab = cex.labs)
    
    if (x.axis != "blank" && is.null(xaxis.labels)) {
      
        tran.datax.tic.location <- 
          SMRD:::f.relationship(datax.tic.location, x.axis)
        good.tran.datax.tic.location <- 
          tran.datax.tic.location[tran.datax.tic.location > -1e+31]
        tran.datax.tic.label.loc <- 
          SMRD:::f.relationship(datax.tic.label.loc, x.axis)
        good.tran.datax.tic.label.loc <- 
          tran.datax.tic.label.loc[tran.datax.tic.label.loc > -1e+31]
        
        axis(side = 1, 
             at = good.tran.datax.tic.location, 
             labels = F,
             tck = -0.01, 
             mgp = c(5, 2.1, 0), 
             cex.axis = par()$cex.axis)
        
        if (!missing(x)) {
            xlabels <- 
              SMRD:::vector.power10(getxax.out$ticlab[tran.datax.tic.label.loc > -1e+31])

            axis(side = 1, 
                 at = good.tran.datax.tic.label.loc,
                 labels = parse(text = SMRD:::fix.exp.labels(getxax.out$ticlab[tran.datax.tic.label.loc > -1e+31])), 
                 adj = 0.5, 
                 tck = -0.02, 
                 mgp = c(5, 1, 0), 
                 cex.axis = cex.tic.lab, 
                 las = 1)
               }
  
        } else {
          
        if (x.axis != "blank" && !is.null(xaxis.labels))
            axis(side = 1, 
                 at = at.xaxis.labels, 
                 labels = abbreviate(xaxis.labels),
                 adj = 0.5, 
                 tck = -0.02, 
                 mgp = c(5, 1, 0), 
                 cex.axis = par()$cex.axis,
                 las = 1)
    
        }
    
    if (y.axis != "blank") {
      
        axis(side = 2, 
             at = SMRD:::f.relationship(datay.tic.location, y.axis), 
             labels = F, 
             tck = -0.01, 
             mgp = c(5, 2.1, 0), 
             cex.axis = par()$cex.axis)
      
        if (!missing(y)) {
          
            ylabels <- SMRD:::vector.power10(getyax.out$ticlab)

            the.labels <- SMRD:::fix.exp.labels(getyax.out$ticlab)
            yaxis.line <- max(nchar(the.labels) - 1, yaxis.line)
            axis(side = 2, 
                 at = SMRD:::f.relationship(datay.tic.label.loc, y.axis), 
                 labels = parse(text = the.labels), 
                 adj = 1, 
                 tck = -0.02,
                 mgp = c(5, 1.1, 0), 
                 cex.axis = par()$cex.axis, 
                 las = 1)

        }
      
  } else {
    
        if (y.axis != "blank" && is.null(yaxis.labels))
            axis(side = 1, 
                 at = at.yaxis.labels, 
                 labels = abbreviate(yaxis.labels),
                 adj = 0.5, 
                 tck = -0.02,
                 mgp = c(5, 1, 0), 
                 cex.axis = par()$cex.axis,
                 las = 1)
  }
    
    if (grids >= 1) {
      
        usr.out <- par("usr")
        yvec.low  <- rep(usr.out[3], 
                         length(datax.tic.label.loc[tran.datax.tic.label.loc > -1e+31]))
        yvec.high <- rep(usr.out[4], 
                         length(datax.tic.label.loc[tran.datax.tic.label.loc >-1e+31]))
        
        matlines(rbind(f.relationship(datax.tic.label.loc[tran.datax.tic.label.loc > -1e+31], x.axis), 
                       f.relationship(datax.tic.label.loc[tran.datax.tic.label.loc > -1e+31], x.axis)), 
                 rbind(yvec.low, yvec.high), 
                 col = "steelblue",
                 lty = 3, 
                 lwd = 1)
        yvec.low  <- rep(usr.out[3], length(datax.tic.location))
        yvec.high <- rep(usr.out[4], length(datax.tic.location))
        matlines(rbind(f.relationship(datax.tic.location, x.axis),
                       f.relationship(datax.tic.location, x.axis)), 
                 rbind(yvec.low, yvec.high), 
                 col = "steelblue", 
                 lty = 3, 
                 lwd = 1)
        
        xvec.low  <- rep(usr.out[1], length(datay.tic.label.loc))
        xvec.high <- rep(usr.out[2], length(datay.tic.label.loc))
        matlines(rbind(xvec.low, xvec.high), 
                 rbind(f.relationship(datay.tic.label.loc, y.axis), 
                       f.relationship(datay.tic.label.loc, y.axis)),
                 col = "steelblue", 
                 lty = 3, 
                 lwd = 1)
        xvec.low  <- rep(usr.out[1], length(datay.tic.location))
        xvec.high <- rep(usr.out[2], length(datay.tic.location))
        matlines(rbind(xvec.low, xvec.high), 
                 rbind(f.relationship(datay.tic.location, y.axis), 
                       f.relationship(datay.tic.location, y.axis)),
                 col = "steelblue", 
                 lty = 3, 
                 lwd = 1)
    }
    
    mtext(side = 2, line = yaxis.line, text = ylab, cex = cex.labs)
    CheckPrintDataName()
    invisible()
}

