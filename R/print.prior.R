print.prior <-
function (x, 
          prefix = "xx", 
          quote = F,...)
{
    parameter.count <- length(x)
    explan.var.count <- length(x) - 2
    relationships <- rep(0, times = explan.var.count)
    xs <- x$quantile$xs
    distribution <- attr(x, "distribution")
    prior.name <- attr(x, "prior.name")
    p <- x$quantile$p
    
    cat("\nPrior distribution", 
        prior.name, 
        "has", 
        distribution,
        p, "quantile as a base\n")
    
    axis.labels <- parameter.contour.axes.labels(x)
    
    if (!is.null(xs)) cat("at x values", xs, "\n")
    
    for (i in 1:parameter.count) {
      
        if (!is.null(x[[i]]$relationship)) {
          
            relationships[i - 1] <- x[[i]]$relationship
        }
      
        spec.out <- x[[i]]$out
        prior.dist <- generic.distribution(spec.out$dist.name)
        
        cat("\nComponent", 
            i, 
            axis.labels[[i]], 
            "has a", 
            prior.dist,
            "distribution with\n")
        
        switch(prior.dist, 
               logbeta = , 
               beta = , 
               loguniform = ,
               uniform = , 
               logtriangle = , 
               triangle = {
        cat("lower and upper limits [", 
            spec.out$lower,
            ", ", 
            spec.out$upper, 
            "]\n", 
            sep = "")
                 
            }, lognormal = , 
               normal = {
         cat("lower and upper 99% limits [", 
             spec.out$lower,
             ", ", 
             spec.out$upper, 
             "]\n", 
             sep = "")
            }, )
    }
}
