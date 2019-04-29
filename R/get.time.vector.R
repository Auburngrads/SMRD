get.time.vector <-
function (obj, 
          distribution, 
          number = 20, 
          time.range = NULL) 
{
    if (is.null(time.range)) {
      
        if (any(CheckClass("mlest", obj))) object.type <- "mlest.out"
        
        if (any(CheckClass("life.data", obj))) object.type <- "life.data"
        if (any(CheckClass("dest.degrad.mle.out", obj))) 
            object.type <- "dest.degrad.mle.out"
        if (any(CheckClass("Dest.Degrad.data", obj))) object.type <- "Dest.Degrad.data"
        
        switch(object.type, life.data = , mlest.out = {
            if (object.type == "mlest.out") {
              
                cdfest.out <- cdfest(obj$data.ld)
                distribution <- obj$distribution
                
            } else {
              
                cdfest.out <- cdfest(obj)
            }
            cdpoints.out <- cdpoints(cdfest.out)
            cdpoints.out <- cdpoints(cdfest.out)
            good.cdfest.out.times <- range(cdfest.out$p[cdfest.out$p > -1e+10], 
                                           cdfest.out$q[cdfest.out$q < 1e+10])
            time.range <- range(cdpoints.out$yplot, 
                                good.cdfest.out.times)
            
        }, dest.degrad.mle.out = , Dest.Degrad.data = {
          
            if (object.type == "Dest.Degrad.data") {
              
                Dest.Degrad.data <- obj
                
            } else {
              
                Dest.Degrad.data <- obj$data.ld
            }
            time.range <- range(times(Dest.Degrad.data))
        }, {
            stop(paste(deparse(substitute(obj)), "is neither class Dest.Degrad.data, dest.degrad.mle.out, life.data nor mlest.out"))
        })
    }
  
    `if`(all(time.range > 0) && max(time.range)/min(time.range) > 2,
         time.vec <- as.numeric(logax(time.range, nint = number)$ticlab),
         time.vec <- as.numeric(linax(time.range, nint = number)$ticlab))
    
    time.vec
}
