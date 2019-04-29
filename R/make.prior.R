make.prior <-
function (specifications.for.prior, 
          number.in.prior) 
{
    distribution <- 
      generic.distribution(attr(specifications.for.prior, "distribution"))
    
    orig.specifications.for.prior <- specifications.for.prior
    parameter.count <- length(specifications.for.prior)
    explan.var.count <- length(specifications.for.prior) - 2
    prior.sample <- matrix(0, 
                           nrow = number.in.prior, 
                           ncol = parameter.count)
    
    relationships <- rep(0, times = explan.var.count)
    
    if (is.logdist(distribution)) {
      
        specifications.for.prior$quantile$out$lower <- 
          logb(orig.specifications.for.prior$quantile$out$lower)
        
        specifications.for.prior$quantile$out$upper <- 
          logb(orig.specifications.for.prior$quantile$out$upper)
        
    }
    
    logsigma <- T
    
    if (logsigma) {
      
        if (distribution == "weibull") {
            specifications.for.prior$sigma$out$lower <- logb(1/orig.specifications.for.prior$sigma$out$upper)
            specifications.for.prior$sigma$out$upper <- logb(1/orig.specifications.for.prior$sigma$out$lower)
        
      } else {
              
            specifications.for.prior$sigma$out$lower <- logb(orig.specifications.for.prior$sigma$out$lower)
            specifications.for.prior$sigma$out$upper <- logb(orig.specifications.for.prior$sigma$out$upper)
        }
    }
    for (i in 1:parameter.count) {
      
        if (!is.null(specifications.for.prior[[i]]$relationship)) {
          
            relationship <- 
              set.relationship.power(specifications.for.prior[[i]]$relationship,              
                                     specifications.for.prior[[i]]$power)

            relationships[i - 1] <- relationship
        }
      
        spec.out <- specifications.for.prior[[i]]$out
        prior.dist <- generic.distribution(spec.out$dist.name)
        
        switch(prior.dist, 
           loguniform = , uniform = {
             prior.sample[, i] <-  spec.out$lower + 
                                  (spec.out$upper - spec.out$lower) * 
                                  runif(number.in.prior)
        }, logtriangle = , triangle = {
             runifvec <- runif(number.in.prior)
             prior.sample[, i] <- ifelse(runifvec < 0.5, spec.out$lower + 
                sqrt(runifvec/2) * (spec.out$upper - spec.out$lower), 
                spec.out$upper + sqrt((1 - runifvec)/2) * (spec.out$lower - 
                  spec.out$upper))
        }, lognormal = , normal = {
            mu <- (spec.out$lower + spec.out$upper)/2
            sigma <- (spec.out$upper - spec.out$lower)/6
            prior.sample[, i] <- rnorm(number.in.prior, mean = mu, 
                sd = sigma)
        }, logbeta = , beta = {
            prior.sample[, i] <- spec.out$lower + (spec.out$upper - 
                spec.out$lower) * rbeta(number.in.prior, spec.out$shapevec[1], 
                spec.out$shapevec[2])
        })
    }
    prior.sample <- as.matrix(prior.sample)
    
    `if`(parameter.count > 2,
         betanames <- paste("beta", 1:(parameter.count - 2), sep = ""),
         betanames <- NULL)
    
    dimnames(prior.sample) <- list(NULL, 
                                   c("quantile", betanames, "sigma"))
    
    xs <- specifications.for.prior$quantile$xs
    p <- specifications.for.prior$quantile$p
    
    if (is.logdist(distribution)) {
        prior.sample[, 1] <- exp(prior.sample[, 1])
    }
    
    prior.sample[, "sigma"] <- exp(prior.sample[, "sigma"])
    
    the.prior <- 
      list(prior = as.matrix(prior.sample), 
           xs = xs, 
           p = p, 
           relationships = relationships, 
           specifications.for.prior = orig.specifications.for.prior, 
           distribution = distribution)
    
    oldClass(the.prior) <- "simulated.points.from.prior"
    return(the.prior)
}
