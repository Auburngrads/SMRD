probplot.setup <-
function (distribution, 
          xlim, 
          ylim, 
          xlab = "Time", 
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          cex = 1,
          cex.title = cex, 
          shape = NULL, 
          my.title = NULL, 
          sub.title = "",
          grids = F, 
          linear.axes = F, 
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          slope.axis = F, 
          draw.line = F, 
          cex.labs = 1.1, 
          hw.xaxis = NULL, 
          hw.yaxis = NULL, 
          dump = T, 
          cex.points = 1,
          title.line.adj = 0, 
          cex.axis = 1.05,
          mar = c(5, 4, 4, 2) + 0.1,
          bty = "o",...)
{
    if (title.option != "blank" && (!is.logical(linear.axes) || (slope.axis))) {
      
        warning("Probably should not have a title if shape parameter scale has been requested")
      
    }
  
    if (missing(xlim)) warning("xlim should be specified")
    if (missing(ylim)) warning("ylim should be specified")
  
    xdiff <- xlim[2] - xlim[1]
    if (xdiff < 0) stop("Requested x-axis upper endpoint less than lower")
    xlim[1] <- xlim[1] + 1e-12 * xdiff
    xlim[2] <- xlim[2] - 1e-12 * xdiff
    
    ydiff <- ylim[2] - ylim[1]
    if (ydiff < 0) stop("Requested y-axis upper endpoint less than lower")
    ylim[1] <- ylim[1] + 1e-12 * ydiff
    ylim[2] <- ylim[2] - 1e-12 * ydiff
    
    GetAxesRange.out <- GetAxesRange("probplot.setup", 
                                     x.axis = "xxx",
                                     xlim, 
                                     xlab, 
                                     y.axis = distribution, 
                                     ylim, 
                                     ylab)
    xlim <- GetAxesRange.out$xlim
    ylim <- GetAxesRange.out$ylim
    
    if (is.logical(linear.axes) && linear.axes) linear.axes <- "b"
    
    get.prob.scales.out <- get.prob.scales(distribution, 
                                           shape = shape,
                                           prob.range = ylim)
    
    log.of.data <- get.prob.scales.out$logger
    
    tick.label.loc <- as.numeric(get.prob.scales.out$tick.labels)
    tick.location <- as.numeric(get.prob.scales.out$tick.location)
    
    `if`(length(grep("Percent", ylab)) > 0,
         show.tick.labels <- get.prob.scales.out$percent.tick.labels,
         show.tick.labels <- get.prob.scales.out$tick.labels)
    
    yp.range <- c(max(min(tick.label.loc), 
                      tick.label.loc[tick.label.loc < ylim[1]]), 
                  min(max(tick.label.loc), 
                      tick.label.loc[tick.label.loc > ylim[2]]))
    
    probplot.setup.title.out <-
      probplot.setup.title(title.option = title.option,
                           my.title = my.title,
                           sub.title = sub.title,
                           distribution = get.prob.scales.out$distribution,
                           shape = shape)
    
    if ((title.option == "paper" || title.option == "paper2") && grids != 0) {
      
        grids <- 2
        
    }
    
    `if`(linear.axes == "q" || linear.axes == "b",
         right.mar <- 6,
         right.mar <- 2)
    
    `if`(slope.axis,
         left.mar <- 8,
         left.mar <- 6)
    
    old.par <- par(err = -1,
                   mar = c(5, 
                           left.mar, 
                           probplot.setup.title.out$top.mar,
                           right.mar) + 0.1)
    
    par(mar = mar, bty = bty)
    
    on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1, err = -1), add = T)
    
    ylim <- pp.quant(yp.range, distribution, shape)
    
    if(is.null(hw.xaxis)) {
      
        `if`(log.of.data,
             data.axes.out <- logax(xlim[1], xlim[2], ...),
             data.axes.out <- linax(xlim))
  
      } else {
        
             data.axes.out <- hw.xaxis
    
      }
    
    xlim <- pp.data(range(xlim, as.numeric(data.axes.out$ticlab)),
                    log.of.data)
    
    if (is.null(hw.yaxis)) {
        tick.location <- 
          tick.location[tick.location >= yp.range[1] &
            tick.location <= yp.range[2]]
        in.range <- 
          tick.label.loc >= yp.range[1] & tick.label.loc <=
            yp.range[2]
        tick.labels <- get.prob.scales.out$tick.labels
        
     } else {
          
        tick.labels <- hw.yaxis$ticlab
        tick.label.loc <- as.numeric(hw.yaxis$ticlab)
        tick.location <- as.numeric(hw.yaxis$ticloc)
        
        tick.location <- 
          tick.location[tick.location >= yp.range[1] &
            tick.location <= yp.range[2]]
        in.range <- 
          tick.label.loc >= yp.range[1] & tick.label.loc <=
            yp.range[2]
    }
    plot(xlim, 
         ylim, 
         xlab = "", 
         ylab = "", 
         xaxt = "n",
         yaxt = "n", 
         type = "n",
         cex.axis = cex.axis, 
         cex.lab = cex.labs,
         main = `if`(T,"",my.title))
    title(xlab = xlab, cex.lab = cex.labs)
    axis(side = 2, 
         at = pp.quant(tick.location, distribution,shape), 
         labels = F, 
         tck = 0.01, 
         mgp = c(5, 2.1, 0),
         cex.axis = cex.axis)
    axis(side = 2, 
         at = pp.quant(tick.label.loc[in.range], distribution, shape), 
         labels = format(show.tick.labels[in.range], scientific = T),
         adj = 1, 
         tck = 0.02, 
         mgp = c(5, 0.5, 0), 
         cex.axis = cex.axis,
         las = 1)
    if (linear.axes == "q" || linear.axes == "b") {
        lin.prob.axes.out <- linax(range(pp.quant(tick.location,
            distribution, shape)))
        data.tick.location <- as.numeric(lin.prob.axes.out$ticloc)
        data.tick.label.loc <- as.numeric(lin.prob.axes.out$ticlab)
        axis(side = 4, 
             at = data.tick.location, 
             labels = F, 
             tck = 0.01,
             mgp = c(5, 2.1, 0), 
             cex = 1)
        axis(side = 4, 
             at = data.tick.label.loc, 
             labels = format(lin.prob.axes.out$ticlab, scientific = T),
             adj = 1, 
             tck = 0.02,
             mgp = c(5, 0.5, 0),
             las = 1, 
             cex.axis = cex.labs)
        mtext(side = 4, 
              line = 3.5, 
              text = paste("Standard", get.prob.scales.out$prob.scale, "Quantile", sep = " "), 
              cex = cex.labs)
    }
    data.tick.location <- as.numeric(data.axes.out$ticloc)
    data.tick.label.loc <- as.numeric(data.axes.out$ticlab)
    axis(side = 1, 
         at = pp.data(data.tick.location, log.of.data),
         labels = F, 
         tck = 0.01, 
         mgp = c(5, 2.1, 0), 
         cex.axis = cex.axis)
    
    xlabels <- vector.power10(data.axes.out$ticlab)
      
      axis(side = 1, 
           adj = 0.5,
           at = pp.data(data.tick.label.loc, log.of.data),
           labels = format(data.axes.out$ticlab, scientific = T), 
           tck = 0.02, 
           mgp = c(5, 0.5, 0), 
           cex.axis = cex.axis)

    if (linear.axes == "t" || linear.axes == "b" && log.of.data) {
        lin.data.axes.out <- 
          linax(range(log10(data.tick.location)))
        lin.data.tick.location <- 
          as.numeric(lin.data.axes.out$ticloc) * 2.302585
        lin.data.tick.label.loc <- 
          as.numeric(lin.data.axes.out$ticlab) * 2.302585
        axis(side = 3, 
             at = lin.data.tick.location, 
             labels = F,
             tck = 0.01, 
             mgp = c(5, 2.1, 0), 
             cex = 1)
        axis(side = 3, 
             at = lin.data.tick.label.loc, 
             labels = format(lin.data.axes.out$ticlab, scientific = T),
             adj = 0.5, 
             tck = 0.02, 
             mgp = c(5, 0.5, 0), 
             cex.axis = cex.labs)
    }
    if (grids >= 1) {
        usr.out <- par("usr")
        yvec.low <- rep(usr.out[3], length(data.tick.location))
        yvec.high <- rep(usr.out[4], length(data.tick.location))
        matlines(x = rbind(pp.data(data.tick.location, log.of.data),
            
                           y = pp.data(data.tick.location, log.of.data)), rbind(yvec.low,
            yvec.high), col = "darkgray", lty = 3, lwd = 1.5, pch = 1)
        xvec.low <- rep(usr.out[1], length(tick.location))
        xvec.high <- rep(usr.out[2], length(tick.location))
        
        matlines(x = rbind(xvec.low, xvec.high), 
                 y = rbind(pp.quant(tick.location,distribution, shape), 
                           pp.quant(tick.location, distribution, shape)), 
                 col = "darkgray", 
                 lty = 3, 
                 lwd = 1.5, 
                 pch = 1)
    }
    if (grids >= 2) {
        yvec.low <- rep(usr.out[3], length(data.tick.label.loc))
        yvec.high <- rep(usr.out[4], length(data.tick.label.loc))
        
        matlines(x = rbind(pp.data(data.tick.label.loc, log.of.data),
            y = pp.data(data.tick.label.loc, log.of.data)), rbind(yvec.low,
            yvec.high), col = "darkgray", lty = 3, lwd = 1.5, pch = 1)
        xvec.low <- rep(usr.out[1], length(tick.label.loc))
        xvec.high <- rep(usr.out[2], length(tick.label.loc))
        matlines(x = rbind(xvec.low, xvec.high), rbind(pp.quant(tick.label.loc,
            distribution, shape), y = pp.quant(tick.label.loc,
            distribution, shape)), col = "darkgray", lty = 3, lwd = 1.5,
            pch = 1)
    }
      
    `if`(!slope.axis,
         mtext(side = 2, 
               line = probplot.setup.title.out$lside.line,
               text = ylab, 
               cex = cex.labs),
         add.slope.scale(distribution, 
                         shape, 
                         draw.line))

     title.line <- probplot.setup.title.out$title.line + title.line.adj
    
      mtext(side = 3,
           line = title.line,
           outer = F,
           text = probplot.setup.title.out$new.title,
           cex = cex.title)

    CheckPrintDataName()
    return(log.of.data)
}
