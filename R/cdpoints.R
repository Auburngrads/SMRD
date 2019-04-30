cdpoints <-
function (cdfest.out, 
          kprint = 0, 
          debug1 = F) 
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
    
    if(debug1) browser()
    
    zout <- WQMPOINTS(as.double(q), 
                      as.double(cdfest.out$p), 
                      as.double(cdfest.out$prob), 
                      as.double(sd), 
                      as.integer(lsd),
                      as.integer(m), 
                      yplot  = double(m + 1), 
                      pplot  = double(m + 1), 
                      sdplot = double(m + 1), 
                      mplot  = integer(1))
    
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
