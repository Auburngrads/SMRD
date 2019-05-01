cdfest <-
function (data.ld, 
          gamthr = 0, 
          kprint = 0, 
          maxit = 5e+05, 
          tol = 0.001,
          maxmsd = 200, 
          start.values = NULL,
          debug1 = F)
{
    y <- Response(data.ld)
    the.case.weights <- case.weights(data.ld)
    
    if (is.null(start.values)) nstart <- 0
    number.cases <- nrow(y)
    ny <- ncol(y)
    the.censor.codes <- censor.codes(data.ld)
    
    if (length(gamthr) == 1)
        gamthr <- rep(gamthr, number.cases)
    
    if (length(gamthr) != number.cases)
        stop("specified offset is the wrong length")
    
    if (ny == 2)
        gamthr <- cbind(gamthr, gamthr)
    
    y <- y - as.matrix(gamthr)
    
    number.observations <- sum(the.case.weights[the.censor.codes > 0])
    left.trun.cond <- NULL
    right.trun.cond <- NULL
    the.truncation.codes <- truncation.codes(data.ld)
    
    if (is.null(the.truncation.codes)) {
      
        ty <- 1
        nty <- 0
        the.truncation.codes <- 1
        
        } else {
          
        ty <- truncation.response(data.ld)
        nty <- ncol(ty)
        if (all(the.truncation.codes == 3))
            left.trun.cond <- min(ty[the.truncation.codes == 3, 1])
        if (all(the.truncation.codes == 2))
            right.trun.cond <- max(ty[the.truncation.codes == 2, nty])
    }
    dummy <- the.censor.codes
    ndscrat <- 3 * number.cases + 4
    nrscrat <- max(7 * (number.cases + 1), 
                   (maxmsd * (maxmsd - 1))/2 + 1)
    niscrat <- 6 * number.cases + 7
    
    if (debug1) browser()
    
    zout <- WQMCDFEST(as.matrix(y),
                      as.integer(ny),
                      as.integer(the.censor.codes),
                      as.integer(the.case.weights),
                      as.matrix(ty),
                      as.integer(nty),
                      as.integer(the.truncation.codes),
                      as.integer(number.cases),
                      as.integer(nstart),
                      double(ndscrat),
                      double(nrscrat),
                      integer(niscrat),
                      as.integer(kprint),
                      as.integer(maxit),
                      as.double(tol),
                      as.integer(maxmsd),
                      p = double(number.cases + 1),
                      q = double(number.cases + 1),
                      prob = double(number.cases + 1),
                      sd = double(number.cases + 1),
                      m = integer(1),
                      pchmax = double(1),
                      lsd = integer(1),
                      ier = integer(1))
    
    lsd <- as.logical(zout$others$lsd)
    m <- zout$others$m
    
    if (zout$others$ier > 0) {
      
        if (zout$others$ier != 21 || map.SMRDDebugLevel() >= 4)
            warning(paste("Cdfest error message", zout$others$ier,
                collapse = " "))
    }
    
    q <- zout$numvec$q
    p <- zout$numvec$p
    prob <- zout$numvec$prob
    length(q) <- m
    length(p) <- m
    length(prob) <- m
    
    if (lsd) {
      
        sd <- zout$numvec$sd
        length(sd) <- m
        rlist <- list(data.ld = data.ld, 
                      p = p, 
                      q = q, 
                      prob = prob,
                      sd = sd, 
                      number.observations = number.observations,
                      left.trun.cond = left.trun.cond, 
                      right.trun.cond = right.trun.cond)
  
     } else {
          
        rlist <- list(data.ld = data.ld,
                      p = p, 
                      q = q, 
                      prob = prob,
                      number.observations = number.observations, 
                      left.trun.cond = left.trun.cond,
                      right.trun.cond = right.trun.cond)
        
        }
    
    oldClass(rlist) <- "ecdf"
    return(rlist)
}
