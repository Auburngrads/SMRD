#' @export
plot.groupi.Dest.Degrad.out <-
function (x,
          transformation.x,
          ylim = c(NA, NA),
          xlim = c(NA, NA),
          my.title = NULL,
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          cex = 1,
          xlab = NULL,
          ylab = NULL,
          lwd = 2,
          pch.vec = (1:(length(unique.stress.clist) + 1))[-2],
          col.vec = (1:(length(unique.stress.clist) + 1)),
          grids = F,
          power,
          focus.variable = 1,
          do.legend = "On plot",
          bty = `if`(grids, "o","L"),...)
{
  CheckString <- function (pat, str) { return(regexpr(pat, str) > 0) }

    transformation.time <- attr(x, "transformation.time")
    increasing <- attr(x, "increasing")
    
    `if`(increasing, sign.factor <- 1, sign.factor <- -1)
    
    transformation.response <- attr(x, "transformation Response")
    y.axis <- "log"
    data.ddd <- attr(x, "data.ddd")
    ok.values <- attr(x, "ok.values")
    stress <- attr(x, "stress")[ok.values, , drop = F]
    
    if (is.character(focus.variable)) {
      
        focus.variable <- match(focus.variable, dimnames(stress)[[2]])
        
    }
    transformation.x <- set.relationship.power(transformation.x,
                                               power)
    
    distribution <- attr(x, "distribution")
    model.string <- paste("Resp:", transformation.response, ",Time:",
                          transformation.time, ",x:", 
                          paste(transformation.x, collapse = ","),
                          ", Dist:", distribution, sep = "")
    
    transformation.x <- fix.inverse.relationship(transformation.x)
    x.axis <- transformation.x
    
    if (is.null(xlab)) {
      
        if (generic.relationship.name(x.axis) == "Box-Cox") {
          
            the.power <- attr(x.axis, "the.power")
            x.axis.name <- paste(x.axis, "(", the.power, ")", sep = "")
            
        } else { 
  
          x.axis.name <- transformation.x 
          
        }
      
        xunits <- dimnames(xmat(data.ddd))[[2]]
        xlab <- paste(xunits[focus.variable], "on", 
                      fix.axis.name(x.axis.name), "Scale")
        
    }
    
    if (is.null(my.title)) {
      
        my.title <- paste("Degradation rate versus", xlab, "for\n",
            get.data.title(data.ddd), "\n", model.string)
        
    }
    
    ok.values <- attr(x, "ok.values")
    stress <- attr(x, "stress")[ok.values, , drop = F]
    slope <- sign.factor * attr(x, "slope")[ok.values]
    slope.lower <- sign.factor * attr(x, "slope.lower")[ok.values]
    slope.upper <- sign.factor * attr(x, "slope.upper")[ok.values]
    
    if (!all(ok.values)) {
      
        warning("Mix of negative and positive\nslopes\nPlotting only the predominant direction")
      
    }
    
    the.names <- dimnames(stress[, -focus.variable, drop = F])[[2]]
    
    if (ncol(stress) > 1) {
      
        the.names <- dimnames(stress[, -focus.variable, drop = F])[[2]]
        stress.clist <- apply(stress[, -focus.variable, drop = F],
                              1, 
                              paste, 
                              sep = "", 
                              collapse = ";", the.names)
        
    } else {
  
        stress.clist <- rep(1, nrow(stress))
        
    }
    
    unique.stress.clist <- unique(stress.clist)
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(stress)[xrna]
    expand.factor <- 1.000001
    xlim <- c(xlim[1] / expand.factor, xlim[2] * expand.factor)
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(slope, slope.lower, slope.upper)[yrna]
    if (is.null(xlab)) xlab <- attr(data.ddd, "x.columns")
    
    on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1))
    
    plot.paper(x = xlim, 
               y = ylim, 
               x.axis = x.axis, 
               y.axis = y.axis,
               xlab = xlab, 
               ylab = "Slope", 
               cex = cex, 
               my.title = "",
               grids = grids, 
               title.option = title.option,
               mar = c(4.5, 5.25, 3.5, 12.1),
               bty = bty,...)
    
    delta.space <- (x.loc(1) - x.loc(0)) / 50
    
    for (i in 1:length(unique.stress.clist)) {
      
        the.ones <- stress.clist == unique.stress.clist[i]
        
        points.default(f.relationship(stress[the.ones, focus.variable], x.axis), 
                       f.relationship(slope[the.ones], y.axis),
                       cex = (1 * GetSMRDDefault("SMRD.point.size"))/100,
                       pch = pch.vec[i], 
                       col = col.vec[i])
        
        y.location.lower <- f.relationship(slope.lower, y.axis)[the.ones]
        segments(f.relationship(stress[the.ones, focus.variable],x.axis) - delta.space, 
                 y.location.lower, 
                 f.relationship(stress[the.ones, focus.variable], x.axis) + delta.space, 
                 y.location.lower,
                 pch = pch.vec[i], 
                 col = col.vec[i])
        
        y.location.upper <- f.relationship(slope.upper, y.axis)[the.ones]
        
        segments(f.relationship(stress[the.ones, focus.variable], x.axis) - delta.space, 
                 y.location.upper, 
                 f.relationship(stress[the.ones, focus.variable], x.axis) + delta.space, 
                 y.location.upper,
                 pch = pch.vec[i], 
                 col = col.vec[i])
        
    }

        if (do.legend == "On plot" && length(unique.stress.clist) > 1) {
          
            par(xpd = T)
          
            legend(x.loc(1.05), 
                   y.loc(0.99), 
                   parse(text = switch.units(unique.stress.clist, NULL)),
                   col = col.vec[1:length(unique.stress.clist)],
                   pch = pch.vec[1:length(unique.stress.clist)], 
                   y.intersp = 1,
                   bty = "n",
                   adj = c(-0.1))
          
        }

    if (CheckString("full", title.option)){
      
        mtext(text = my.title, side = 3, cex = 1.2, line = 0.5)
      
    }
    
    invisible()
    
}
