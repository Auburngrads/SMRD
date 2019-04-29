cdpoints <-
function (cdfest.out, kprint = 0, debug1 = F) 
{
    q <- cdfest.out$q
    sd <- cdfest.out$sd
    if (is.null(sd)) {
      
          sd <- rep(1, length(q))
          lsd <- 0
        
        } else { 
      
          lsd <- 1
    
        }
    if (length(q) == 0) {
        warning("no cdf estimate available")
        return(NULL)
    }
    m <- length(q)
    if (debug1) 
        browser()
    zout <- .Fortran("wqmpoints", 
                     as.single(q), 
                     as.single(cdfest.out$p), 
                     as.single(cdfest.out$prob), 
                     as.single(sd), 
                     as.integer(lsd),
                     as.integer(m), 
                     yplot = single(m + 1), 
                     pplot = single(m + 1), 
                     sdplot = single(m + 1), 
                     mplot = integer(1))
      # wqmpoints(q = as.double(q),
      #                 p = as.double(cdfest.out$p),
      #                 prob = as.double(cdfest.out$prob),
      #                 sd = as.double(sd),
      #                 lsd = as.integer(lsd),
      #                 m = as.integer(m))
    yplot <- zout$yplot
    pplot <- zout$pplot
    mplot <- zout$mplot
    length(yplot) <- mplot
    length(pplot) <- mplot
    if (lsd == 1) {
      
        sdplot <- zout$sdplot
        length(sdplot) <- mplot
        return(list(yplot = yplot, 
                    pplot = pplot, 
                    sdplot = sdplot, 
                    number.observations = cdfest.out$number.observations,
                    left.trun.cond = cdfest.out$left.trun.cond, 
                    right.trun.cond = cdfest.out$right.trun.cond))
        
        } else {
      
        return(list(yplot = yplot, 
                    pplot = pplot, 
                    number.observations = cdfest.out$number.observations, 
                    left.trun.cond = cdfest.out$left.trun.cond, 
                    right.trun.cond = cdfest.out$right.trun.cond))
    
        }
}
