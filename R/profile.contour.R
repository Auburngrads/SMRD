profile.contour <-
function (fitted, 
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          profile.title = paste(fitted$subtitle, "\n", lplot.type, "Likelihood for", 
                                variable.namex, "and", variable.namey, "\n", model.dist.str), 
          variable.namex = fitted$xlab,
          variable.namey = fitted$ylab, 
          transformationx = "linear",
          transformationy = "linear", 
          original.par = F, 
          levels = c(0.001, 0.01, 0.2, 0.4, 0.6, 0.8, 0.9), 
          pretty.x = NULL, 
          pretty.y = NULL, 
          add = F, 
          lty = 1, 
          col = 1, 
          lwd = 1,...)
{
  
    do.mixed.text <- is.postsctiptok() && substring(variable.namex, 1, 1) == "~"
    
    `if`(!is.null(fitted$number.parameters) && fitted$number.parameters == 2,
         lplot.type <- "Relative",
         lplot.type <- "Profile")
    
    model.dist.str <- NULL
    
    if(is.null(fitted$distribution)) {
      
       `if`(is.null(fitted$form),
            model.dist.str <- NULL,
            model.dist.str <- paste("from the", fitted$form, " Model"))
      
     } else {
       
       `if`(is.null(fitted$form),
            model.dist.str <- paste("from the", fitted$distribution, "Distribution"),
            model.dist.str <- paste("from the", fitted$distribution, fitted$form, " Model"))
       
     }
 
    if(!add) {
      
        par(new = F)
        par(mar = c(4.5, 5, 3.5, 4) + 0.1)
        old.par <- par(mar = c(4.5, 5, 3.5, 2) + 0.1, cex = 1.1)
        if (original.par) on.exit(par(old.par))
        
    }
    cex.lab <- 1.1
    
    if(!add) {
      
       xrna <- is.na(xlim)
       if(any(xrna)) xlim[xrna] <- range(fitted$x)[xrna]
       yrna <- is.na(ylim)
       if(any(yrna)) ylim[yrna] <- range(fitted$y)[yrna]
        #plot(xlim, ylim, type = "n", xaxt = "n", yaxt = "n",  xlab = "", ylab = "")
       
    }
    
    plot3D::image2D(z = fitted$z, 
                    x = fitted$x,
                    y = fitted$y, 
                    levels = levels, 
                    lty = lty,
                    lwd = lwd, 
                    contour = TRUE, 
                    clab = c(expression(frac(bolditalic(L)[0],bolditalic(L)[MLE]))),
                    xlab = parse(text = variable.namex), 
                    ylab = parse(text = variable.namey),
                    cex.lab = cex.lab)
    
    if(add) return(invisible())
    if(do.mixed.text) {
      
        mixed.mtext(side = 1, 
                    texts = variable.namex, 
                    line = 1.5,
                    cex = 1.5 * cex.lab)
        mixed.mtext(side = 2, 
                    line = 2, 
                    texts = variable.namey,
                    srt = 0, 
                    cex = 1.5 * cex.lab)
        
      } else {
        
        #title(xlab = parse(text = variable.namex), cex.lab = 1.1)
        #mtext(side = 2, line = 3.5, parse(text = variable.namey), cex = 1.1)
        
      }
    
    if(is.null(pretty.x)) {
      
        `if`(transformationx == "log",
             { trans.range <- f.relationshipinv(xlim, transformationx)
               pretty.x <- logax(trans.range[1], trans.range[2])$ticlab },
             { pretty.x <- pretty.check(wqm.pretty(f.relationshipinv(xlim, transformationx), nint = 6), 
                                        transformationx) })
      
    }
    #axis(side = 1, at = f.relationship(as.numeric(pretty.x),
    #    transformationx), labels = format(pretty.x), cex.axis = 1.1,tck = -0.01, line = -3.75)
    if(is.null(pretty.y)) {
      
       pretty.y <- pretty.check(wqm.pretty(f.relationshipinv(ylim, transformationy), nint = 6), 
                                transformationy)
       
    }
    #axis(side = 2, at = f.relationship(as.numeric(pretty.y),
    #    transformationy), labels = format(pretty.y), adj = 1, tck = -0.01, 
    #    cex.axis = 1.1, las = 1, line = -4.2)
    
        line.adj <- -2
  
    #mtext(profile.title, side = 3, outer = F, line = 4 + line.adj,  cex = 1.1)
    CheckPrintDataName()
    invisible()
    
}
