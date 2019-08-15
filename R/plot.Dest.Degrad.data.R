#' @export
plot.Dest.Degrad.data <-
function (x, 
          transformation.response = "log", 
          transformation.time = "linear",
          ylim = c(NA, NA), 
          xlim = c(NA, NA), 
          my.title = NULL,
          ylab = NULL, 
          xlab = NULL, 
          cex = 1.05, 
          cex.labs = 1.05, 
          cex.points = 1,
          add = F, 
          grids = F, 
          title.option = NULL, 
          pch.point = NULL,
          response.on.yaxis = T, 
          group.var = 1:ncol(xmat(x)),
          subset = T, 
          fail.level = NULL, 
          do.legend = "On plot", 
          do.plot = T,
          mar = c(4.5, 5.25, 3.5, 12.1),
          bty = `if`(grids, "o", "L"),...)
{

`do.list<-` <- function (data.ld, value) { 
  
    attr(data.ld, "do.list") <- value

    return(data.ld)  
  }

    subset <- get.subset.vector(subset, x)
    attr.data <- attributes(x)
    subset.name <- attr(subset, "subset.name")
    x <- x[subset, ]
    relationship.sanity(times(x), 
                        transformation.time,
                        "Transformation for Time")
    
    relationship.sanity(Response(x), 
                        transformation.response,
                        "Transformation for Response")
    
    if (is.null(ylab)) ylab <- get.response.units(x)
    
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(Response(x))[yrna]
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(times(x))[xrna]
    
    Response(x) <- 
      as.matrix(f.relationship(Response(x),
                               transformation.response))
    
    times(x) <- 
      f.relationship(times(x), transformation.time)
    
    theResponse <- Response(x)
    model.string <- paste("Resp:", 
                          transformation.response, 
                          ",Time:",
                          transformation.time, sep = "")
    
    if (is.null(my.title)) {
      
        my.title <- paste(get.data.title(x), 
                          "\nDestructive Degradation",
                          " Scatter Plot\n", 
                          model.string, sep = "")
        
    }
    
    if (any(is.na(group.var)) || is.null(xmat(x))) {
      
        the.xmat <- matrix(rep(1, length = nrow(Response(x))))
        do.list(x) <- 1
        
        } else {
          
        the.xmat <- as.matrix(xmat(x)[, group.var, drop = F])
        xmat(x) <- the.xmat
        do.list(x) <- get.x.markers(x, 
                                    group.var = group.var,
                                    long = T, 
                                    include.complete = T)
        
        complete.list(x) <- complete.list(do.list(x))
        hold.warn <- options(warn = 0)
        tmp.orig.param <- two.stage.dest.degrad(x, distribution = "normal")
        options(hold.warn)
        slope <- attr(tmp.orig.param, "slope")
        
        }
    
    the.do.list <- do.list(x)
    the.times <- times(x)
    if (is.null(xlab)) xlab <- get.time.units(x)
    the.censor.codes <- censor.codes(x)
    the.case.weights <- case.weights(x)
    transformation.time.name <- transformation.time
    
    if (generic.relationship.name(transformation.time) == "Arrhenius") {
      
        transformation.time <- "Arrhenius3"
        transformation.time <- 
          set.relationship.power(transformation.time,
                                 power)
        
    }
    
    transformation.response.in <- transformation.response
    transformation.response <- 
      fix.inverse.relationship(transformation.response.in)
    
    if (!is.null(title.option) && length(title.option) > 0 &&
        title.option == "blank")
        my.title <- ""
    
    if (!do.plot) return(x)
    
    on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1), add = T)
    
    if (!add) plot.paper(x = xlim, 
                         y = ylim, 
                         x.axis = transformation.time,
                         y.axis = transformation.response, 
                         ylab = ylab, 
                         xlab = xlab,
                         response.on.yaxis = response.on.yaxis, 
                         cex = cex,
                         cex.labs = cex.labs, 
                         grids = grids,
                         mar = mar,
                         bty = bty,...)

    #mtext(text = my.title, side = 3, cex = 1.2, line = 0.5)
    dummy.obs <- the.censor.codes == 0 | the.case.weights == 0
    
    if (is.null(pch.point)) pch.point <- (1:(length(the.do.list) + 4))[-c(2, 6, 17, 19)]
    
    if (length(pch.point) == 1) pch.point <- rep(pch.point, length(the.do.list))
    
    for (i in 1:length(the.do.list)) {
      
        `if`(!is.null(complete.list(x)),
             the.ones <- the.do.list[i] == complete.list(x),
             the.ones <- rep(T, length(the.censor.codes)))
      
        rcensored <- the.ones & the.censor.codes == 2 & !dummy.obs
        ncensored <- the.ones & the.censor.codes == 1 & !dummy.obs
        lcensored <- the.ones & the.censor.codes == 3 & !dummy.obs
        icensored <- the.ones & the.censor.codes == 4 & !dummy.obs
        
        if (response.on.yaxis) {
          
            if (any(ncensored))
                points.default(as.vector(the.times[ncensored]),
                               theResponse[ncensored, 1], 
                               pch = pch.point[i],
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
          
            if (any(rcensored))
                points.default(as.vector(the.times[rcensored]),
                               theResponse[rcensored, 1], 
                               pch = 17, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
          
            if (any(lcensored))
                points.default(as.vector(the.times[lcensored]),
                               theResponse[lcensored, 1], 
                               pch = 19, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
          
            if (any(icensored))
                points.default(as.vector(the.times[icensored]), 
                               (theResponse[icensored, 1] + theResponse[icensored,2])/2, 
                               pch = 4, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
          
 
          } else {
            
            if (any(ncensored))
                points.default(theResponse[ncensored, 1], 
                               as.vector(the.times[ncensored]), 
                               pch = pch.point[i], 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
            
            if (any(rcensored)) {
              
                rh.eps <- 0.001
                rh.size <- 0.1
                arrows(theResponse[rcensored, 1] - rh.eps, 
                       as.vector(the.times[rcensored]),
                       theResponse[rcensored, 1] + rh.eps, 
                       as.vector(the.times[rcensored]),
                       size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100,
                       open = F, 
                       rel = F, 
                       col = i)
                
            }
            
            if (any(lcensored)) {
              
                rh.eps <- 0.001
                rh.size <- 0.1
                arrows(theResponse[lcensored, 1] + rh.eps, 
                       as.vector(the.times[lcensored]), 
                       theResponse[lcensored, 1] - rh.eps, 
                       as.vector(the.times[lcensored]), 
                       size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100,
                       open = F, 
                       rel = F, 
                       col = i)
                
            }
            
            if (any(icensored))
                points.default((theResponse[icensored, 1] + theResponse[icensored, 2])/2, 
                               as.vector(the.times[icensored]),
                               pch = 4, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = i)
        
            }
    }
    
    if (!is.null(fail.level)) abline(h = f.relationship(fail.level, transformation.response),
                                     lwd = 3)
    
    if (any(is.na(group.var))) return(x)
    
    `if`(mean(slope) > 0,
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.99) },
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.99) })

        if (length(the.do.list) > 1) {
          
            if (do.legend == "On plot") {
              
                par(xpd = T)
              
                legend(legend.x, 
                       legend.y, 
                       parse(text = switch.units(the.do.list, NULL)), 
                       cex = cex,
                       bty = "n", 
                       col = 1:length(the.do.list), 
                       pch = pch.point,
                       y.intersp = 1,
                       adj = c(-0.1))
              
            }
          
            if (do.legend == "New page" || do.legend == "New file") {
              
                if (do.legend == "New file")
                  postscript(file = "Save_legend.ps", horizontal = T)
              
                plot(c(0, 0), 
                     c(1, 1), 
                     xlab = "", 
                     ylab = "",
                     type = "n", 
                     xaxt = "n", 
                     yaxt = "n")
                
                legend(x.loc(0.003), 
                       y.loc(0.997), 
                       the.do.list,
                       cex = 1.1, 
                       bty = "n", 
                       col = 1:length(the.do.list),
                       pch = pch.point, 
                       y.intersp = 0.675)
                
                if (do.legend == "New file") dev.off()
                
            }
        }

    invisible(x)
}