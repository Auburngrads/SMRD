specify.simple.prior <-
function (p, 
          qdist, 
          qlower, 
          qupper, 
          sigma.dist, 
          sigma.lower, 
          sigma.upper, 
          distribution) 
{
    the.prior <- 
      list(quantile = list(p = p, 
                           out = list(dist.name = qdist, 
                                      lower = qlower, 
                                      upper = qupper)), 
        
           sigma =    list(2, 
                           out = list(dist.name = sigma.dist, 
                                      lower = sigma.lower, 
                                      upper = sigma.upper)))
    
    attr(the.prior, "distribution") <- distribution
    attr(the.prior, "date") <- date()
    oldClass(the.prior) <- "prior"
    
    return(the.prior)
}
