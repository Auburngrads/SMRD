#' @export
plot.rflm.mle <-
function (x, 
          ylim = c(NA, NA), 
          xlim = c(NA, NA),
          my.title = NULL, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          censor.time = NULL,
          density.scale = NULL, 
          density.at = "Automatic", 
          add.density.at = NULL,
          quant.lines = c(0.1, 0.5, 0.9), 
          cex = 1, 
          nxpoints = 50, 
          xlab = NULL,
          ylab = NULL, 
          lwd = 2, 
          grids = F, 
          add = F, 
          response.on.yaxis = T,
          include.data = T, ...)
{
  
    data.ld <- x$data.ld
    
    xunits <- get.xlabel(data.ld)
    yunits <- get.time.units(data.ld)
    x.axis <- "log"
    y.axis <- "log"
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(xmat(data.ld)[, 1])[xrna]
    if (is.null(my.title)) my.title <- get.data.title(data.ld)
    expand.factor <- 1.000001
    xlim <- c(xlim[1]/expand.factor, xlim[2] * expand.factor)
    txlim <- xlim
    xvec <- seq(txlim[1], txlim[2], length = nxpoints)
    
    if (!add) {
      
        if (is.null(xlab)) xlab <- paste(xunits)
        if (is.null(ylab)) ylab <- yunits
        logdist <- T
        ylim.data <- range(Response(data.ld), censor.time)
        yrna <- is.na(ylim)
        if (any(yrna)) ylim[yrna] <- ylim.data[yrna]
        
        plot.paper(x = xlim, 
                   ylim, 
                   x.axis = x.axis, 
                   y.axis = y.axis,
                   xlab = xlab, 
                   ylab = ylab, 
                   response.on.yaxis = response.on.yaxis,
                   cex = cex, 
                   my.title = "", 
                   grids = grids, 
                   title.option = title.option,...)
        
    }
    
    if (!is.null(censor.time)) {
      
        `if`(response.on.yaxis,
             abline(h = f.relationship(censor.time, y.axis), col = 6, lwd = 3),
             abline(v = f.relationship(censor.time, y.axis), col = 6, lwd = 3))
      
    }
    rflm.model.lines.out <- rflm.model.lines(x, pts = nxpoints)
    quant.names <- matrix(NA, nrow = length(quant.lines), ncol = 1)
    
    for (i in 1:length(quant.lines)) {
      
      quant.names[i] <- c(paste("t[",quant.lines[i],"]"))
      
        if (response.on.yaxis) {
          
            lines(f.relationship(rflm.model.lines.out$xvec[-nxpoints],x.axis), 
                  rflm.model.lines.out$yquant.mat[-nxpoints,i], 
                  lwd = lwd, 
                  col = i + 1, 
                  lty = i)
          
#             text(f.relationship(rflm.model.lines.out$xvec[nxpoints],
#                 x.axis), rflm.model.lines.out$yquant.mat[nxpoints,
#                 i], percent.conf.level(quant.lines[i]), adj = -0.5)
      } else {
        
            lines(rflm.model.lines.out$yquant.mat[-nxpoints,i], 
                  f.relationship(rflm.model.lines.out$xvec[-nxpoints],x.axis), 
                  lwd = lwd, 
                  col = i + 1, 
                  lty = i)
        
#             text(rflm.model.lines.out$yquant.mat[1, i], y.loc(0.05),
#                 percent.conf.level(quant.lines[i]), adj = 0.5)
        
        }
    }
    
    legend("topright", 
           legend = parse(text = quant.names),
           lwd = lwd, 
           col = seq(2,length(quant.lines) + 1),
           lty = seq(1,length(quant.lines)), 
           bty = "n")
    
    #title(main = my.title)
    if (include.data && !is.null(data.ld) && is.onlist("life.data", oldClass(data.ld))){
      
        censored.data.plot(data.ld, 
                           x.axis = x.axis, 
                           y.axis = y.axis,
                           add = T, 
                           title.option = title.option, 
                           my.title = my.title,
                           response.on.yaxis = response.on.yaxis)
      
    }
    
    invisible()
    
}