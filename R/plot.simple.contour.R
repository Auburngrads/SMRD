#' @export
plot.simple.contour <-
function (x, 
          the.quantile = NA, 
          rel.or.conf = "Joint confidence region",
          contour.indicators = NULL,
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          factor = 3.5,
          show.mle = T,
          my.title = NULL,
          do.persp = F,
          direction = NULL,
          elevation = NULL,
          distance = NULL,
          original.par = T,
          add = add,
          lwd = 1,
          static = static,...)
{
    if (original.par) {
      
        old.par <- par()
        on.exit({
            old.par[c("cin", "cra", "csi", "cxy", "din", "page")] <- NULL
            par(old.par)
            par(new = F)
        })
        
    }
    distribution <- generic.distribution(attr(x, "distribution"))
    
    if ((distribution == "weibull" && x$xlab == "mu") || x$log.quantile) {
      
        transformationx <- "log"
        
        if (distribution == "weibull" && x$xlab == "mu") {
          
            `if`(do.persp,  x$xlab <- "log(eta)", x$xlab <- "eta")
          
        }
        
    } else { transformationx <- "linear" }
    
    data.title <- attr(x, "title")
    dist.name <- distribution.name(distribution)
    
    if (do.persp) {
      
        if (is.null(my.title)) my.title <- paste(data.title, 
                                                 "\n", 
                                                 dist.name, 
                                                 "Distribution Relative Likelihood")
        
        if (any(is.null(c(direction, elevation, distance)))) {
          
            theta <- 40
            phi <- 20
            r <- sqrt(3)
            
      } else {
        
            theta <- direction
            phi <- elevation
            r <- distance
            
      }
        
        the.ranges <- sapply(x[c(1, 2, 3)], function(x) diff(range(x, na.rm = T)))
        
        if (map.SMRDDebugLevel() >= 4) cat("\nView = ", 
                                           paste(direction,
                                                 elevation, 
                                                 distance, 
                                                 sep = ":"), 
                                           "\n")
        
        likelihood.perspective(x, 
                               profile.title = "",
                               transformationy = "linear", 
                               theta = theta, 
                               phi = phi,
                               r = r,
                               static = static)
        
  } else {
    
        if (rel.or.conf == "Joint confidence region") {
          
            if (is.null(my.title)) my.title <- paste(data.title, 
                                                     "\n", 
                                                     dist.name,
                                                     "Distribution Joint Confidence Region")
            
            if (is.null(contour.indicators)) {
              
                contour.indicators <- c(25, 50, 60, 70, 80, 90, 95, 99)
                
            }
            
            xlim <- attr(x, "xlim")
            ylim <- attr(x, "ylim")
            
            conf.contour(x, 
                         xlim = xlim,
                         ylim = ylim, 
                         profile.title = "", 
                         transformationx = transformationx,
                         variable.namey = x$ylab, 
                         transformationy = "linear", 
                         lwd = lwd,
                         levels = contour.indicators, 
                         add = add, 
                         original.par = F,
                         static = static,...)
            
      } else {
        
            if (is.null(my.title)) my.title <- paste(data.title,
                                                     "\n",
                                                     dist.name,
                                                     "Distribution Relative Likelihood")
            
            if (is.null(contour.indicators)) {
              
                contour.indicators <- c(0.1, 0.2, 0.3, 0.5, 0.9)
                
            }
            
            profile.contour(fitted = x, 
                            profile.title = "",
                            transformationx = transformationx, 
                            variable.namey = x$ylab,
                            transformationy = "linear", 
                            levels = contour.indicators,
                            add = add, 
                            original.par = F, 
                            lwd = lwd,
                            static = static)
            
      }
    
        if (show.mle & static) {
          
            quanthat <- attr(x, "quanthat")
            spread.hat <- attr(x, "spread.hat")
            arrows(quanthat, spread.hat, quanthat, y.loc(0), length = 0.15, lwd = 1)
            arrows(quanthat, spread.hat, x.loc(0), spread.hat, length = 0.15, lwd = 1)
            
        }
    
  }
    
    if (title.option == "full" & static) title(my.title, cex = 0.8)
    invisible(x)
    
}
