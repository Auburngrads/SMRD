quant <-
function (p, distribution) 
{
  
    for(i in seq_along(p)){
            
        if((p[i] < 0) & (abs(p[i] - 0) < 1e-6)) p[i] <- 0
        if((p[i] > 1) & (abs(p[i] - 1) < 1e-6)) p[i] <- 1
            
    }
  
    if (any(p < 0) | any(p > 1)) {
      
        stop(paste("\nquantiles must be between 0 and 1", p))
      
    }
  
    switch(generic.distribution(distribution), 
           weibull     = { qvec <- logb(-logb(1 - p)) }, 
           sev         = { qvec <- logb(-logb(1 - p)) }, 
           uniform     = { qvec <- p }, 
           loguniform  = { qvec <- p }, 
           frechet     = { qvec <- -logb(-logb(p)) }, 
           lev         = { qvec <- -logb(-logb(p)) }, 
           normal      = { qvec <- qnorm(p) }, 
           lognormal   = { qvec <- qnorm(p) }, 
           loglogistic = { qvec <- qlogis(p) }, 
           logistic    = { qvec <- qlogis(p) }, 
           exponential = { qvec <- qexp(p) }, 
           
           {
           
           stop(paste(distribution, "is unrecognized distribution in quant()"))
             
           })
  
    return(qvec)
  
}
