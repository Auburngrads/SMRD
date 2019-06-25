get.parametric.quantiles <-
function (mlest.out, 
          prob.vec = as.numeric(c(".001", ".005", ".01", ".05", ".1", 
                                  ".2", ".3", ".4", ".5", ".6", ".7", 
                                  ".8", ".9", ".99")), 
          conf.level, 
          shape = NULL, 
          do.ci = T)
{
    distribution <- mlest.out$distribution
    theta.hat <- mlest.out$theta.hat
    mu    <- theta.hat[1]
    sigma <- theta.hat[2]
    
    `if`(generic.distribution(distribution) == "exponential",
         evdistribution <- "Weibull",
         evdistribution <- distribution)
    
    zquan <- quant(prob.vec, evdistribution)
    tquanhat <- mu + zquan * sigma
    
    if (missing(conf.level)) {
      
        if (is.logdist(distribution)) tquanhat <- exp(tquanhat)
        return(tquanhat)
        
    }
    
    if (do.ci) {
      
        zvalue <- qnorm(1 - (1 - conf.level)/2)
        zmat <- cbind(1, zquan)
        vartquan <- diag(zmat %*% mlest.out$vcv.matrix %*% t(zmat))
        stderror <- sqrt(vartquan)
        lower <- tquanhat - zvalue * stderror
        upper <- tquanhat + zvalue * stderror
        
        if (is.logdist(distribution)) {
          
            tquanhat <- exp(tquanhat)
            lower <- exp(lower)
            upper <- exp(upper)
            stderror <- stderror * tquanhat
            
        }
        
        band.type <- "parametric pointwise"
        
        return(list(prob.vec = prob.vec, 
                    tquanhat = tquanhat,
                    stderror = stderror, 
                    lower = lower, 
                    upper = upper))
        
    } else {
      
        if (is.logdist(distribution)) tquanhat <- exp(tquanhat) 
        return(tquanhat)
        
    }
    
}
