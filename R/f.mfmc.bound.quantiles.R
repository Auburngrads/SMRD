f.mfmc.bound.quantiles <-
function (multiple.mlest.out, 
          prob.vec, 
          number.times = 10, 
          number.try = 15) 
{
    theta.hat <- unlist(lapply(multiple.mlest.out, 
                               function(sublist) { sublist[["theta.hat"]] }))
    
    log.of.data <- all(unlist(lapply(multiple.mlest.out, 
                                     function(sublist) { is.logdist(sublist[["distribution"]]) })))
    
    prob.range <- range(prob.vec)
    
    if (prob.range[1] > 0.1) prob.range[1] <- 0.1
    if (prob.range[2] < 0.9) prob.range[2] <- 0.9
    
    lower.quant.lower.approx <- NULL
    upper.quant.lower.bound <- NULL
    for (i in 1:length(multiple.mlest.out)) {
        
        quant.now <- get.parametric.quantiles(multiple.mlest.out[[i]], 
                                              prob.range)
        
        lower.quant.lower.approx <- min(lower.quant.lower.approx,
                                        quant.now[1])
        
        upper.quant.lower.bound <- min(upper.quant.lower.bound,
                                       quant.now[2])
        
    }
    
    for (i in 1:number.try) {
        
        `if`(log.of.data,
             lower.quant.lower.try <- lower.quant.lower.approx / 2,
             lower.quant.lower.try <- lower.quant.lower.approx - 0.25 * (upper.quant.lower.bound - lower.quant.lower.approx))
        
        time.vec <- seq(lower.quant.lower.try, 
                        upper.quant.lower.bound, 
                        length = number.times)
        
        try.probs <- fx.mfmc.probs(theta.hat, 
                                   multiple.mlest.out, 
                                   time.vec)
        if (F) {
            
            cat("quant approx = ", 
                format(lower.quant.lower.try), 
                format(lower.quant.lower.approx),
                format(upper.quant.lower.bound), "\n")
            cat("\nthe.times = ", paste(format(time.vec), collapse = ","))
            cat("\nthe.probs = ", paste(format(try.probs), collapse = ","), "\n")
            
        }
        
        upper.quant.lower.bound <- min(time.vec[try.probs > prob.range[2]])
        
        if (min(try.probs) < prob.range[1]) {
            
            lower.quant.lower.bound <- max(time.vec[try.probs < prob.range[1]])
            return(c(lower.quant.lower.bound, upper.quant.lower.bound))
            
        }
        
        lower.quant.lower.approx <- min(time.vec)
        
    }
    
}