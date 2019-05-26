trunc.adj.cdfest.out <-
function (x, 
          mlest.out,
          debug1 = T,...)
{
    right.trun.cond <- x$right.trun.cond
    left.trun.cond <- x$left.trun.cond
    log.of.data <- is.even(numdist(mlest.out$distribution))
    
    if (debug1) {
      
        xx <- list(lower = x$p, 
                   upper = x$q,
                   prob = x$prob, 
                   sd = x$sd)
        print(xx)
        
    }
    
    xrange = range(Response(mlest.out$data.ld))
    
    if (!is.null(left.trun.cond)) {
      
        plot.quantiles.out <- plot.quantiles(mlest.out, 
                                             conf.level = 0,
                                             xrange = c(left.trun.cond,xrange[2]), 
                                             plotem = F, 
                                             timelen = 1,
                                             log.of.data = log.of.data)
        
        ftlower <- plot.quantiles.out$dist.probs
        se.ftlower <- plot.quantiles.out$stderror
        
  } else {
    
        ftlower <- 0
        se.ftlower <- 0
        
  }
    
    if (!is.null(right.trun.cond)) {
      
        plot.quantiles.out <- plot.quantiles(mlest.out, 
                                             conf.level = 0,
                                             xrange = c(xrange[1],right.trun.cond),
                                             plotem = F, 
                                             timelen = 1,
                                             log.of.data = log.of.data)
        
        ftupper <- plot.quantiles.out$dist.probs
        se.ftupper <- plot.quantiles.out$stderror
        
  } else {
    
        ftupper <- 1
        se.ftupper <- 0
        
  }
    
    x$prob <- (ftlower + (ftupper - ftlower) * x$prob)
    correct.vec <- c(ftlower = ftlower, 
                     left.trun.cond = left.trun.cond,
                     ftupper = ftupper, 
                     right.trun.cond = right.trun.cond)
    
    if (debug1) {
      
        print(correct.vec)
        xx <- list(lower = x$p, 
                   upper = x$q,
                   x$prob, 
                   left.trun.cond = x$left.trun.cond,
                   right.trun.cond = x$right.trun.cond, 
                   sd = x$sd)
        print(xx)
        
    }
    
    return(x)
    
}
