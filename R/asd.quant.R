asd.quant <-
function (plan.values, 
          n, 
          censor.time, 
          quantile.mark) 
{
    distribution <- basic.distribution(plan.values$distribution)
    
    if (any(quantile.mark <= 0) || any(quantile.mark >= 1)) {
        
        stop("Specified quanttile not between 0-1.")
        
    }
    zc <- (logb(censor.time) - plan.values$mu) / plan.values$sigma
    ze <- quant(quantile.mark, plan.values$distribution)
    vlength <- max(length(zc), length(ze))
    zc <- expand.vec(zc, vlength)
    ze <- expand.vec(ze, vlength)
    
    zout <- VAVAR(as.integer(idist),
                  as.integer(vlength),
                  as.double(zc),
                  as.double(ze),
                  double(vlength))
    
    return(sqrt(zout$avar / n) * plan.values$sigma)
}
