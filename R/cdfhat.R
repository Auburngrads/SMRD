cdfhat <-
function (mlest.out, time.vec) 
{
    distribution <- mlest.out$distribution
    theta.hat <- mlest.out$theta.hat
    
    if (is.logdist(distribution)) {
      
        ltimes <- logb(time.vec)
        
        } else {
          
        ltimes <- time.vec
    
        }
    z <- (ltimes - theta.hat[1])/theta.hat[2]
    fhat <- wqmf.phibf(z, distribution)
    
    return(list(time.vec = time.vec, fhat = fhat))
}
