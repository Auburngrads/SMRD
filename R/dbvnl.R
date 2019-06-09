dbvnl <-
function (ax, 
          ay, 
          mux = 0, 
          muy = 0, 
          sdx = 1, 
          sdy = 1, 
          rho) 
{
    number <- max(length(ax), 
                  length(ay),
                  length(sdx), 
                  length(sdy),
                  length(mux), 
                  length(muy), 
                  length(rho))
    
    ax  <- expand.vec(ax, number)
    ay  <- expand.vec(ay, number)
    mux <- expand.vec(mux, number)
    muy <- expand.vec(muy, number)
    sdx <- expand.vec(sdx, number)
    sdy <- expand.vec(sdy, number)
    rho <- expand.vec(rho, number)
    one.minus.r2 <- 1 - rho^2
    xpart <- ((ax - mux) / sdx)
    ypart <- ((ay - muy) / sdy)
    xypart <- -2 * rho * xpart * ypart
    rho.fact <- -1 / (2 * (one.minus.r2))
    log.density <- -logb(2 * pi * sdx * sdy * sqrt(one.minus.r2)) + 
        rho.fact * (xpart^2 + ypart^2 + xypart)
    
    return(log.density)
    
}
