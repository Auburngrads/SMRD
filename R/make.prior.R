#' Title
#'
#' @param specifications.for.prior 
#' @param number.in.prior 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using a non-informative quantile 
#' # and a noninformative sigma
#' 
#' prior.spec1 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "loguniform",
#'                        qlower = 100,
#'                        qupper = 5000,
#'                        sigma.dist =  "lognormal",
#'                        sigma.lower = 0.2,
#'                        sigma.upper =  0.5,
#'                        distribution = "Weibull")
#'                        
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using a noninformative quantile 
#' # and an informative sigma
#' 
#' prior.spec2 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "loguniform",
#'                        qlower = 1000,
#'                        qupper = 1400,
#'                        sigma.dist = "lognormal",
#'                        sigma.lower = 1.5,
#'                        sigma.upper = 2.5, 
#'                        distribution  = "Weibull")
#'                        
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using an informative quantile 
#' # and an informative sigma
#' 
#' prior.spec3 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "lognormal",
#'                        qlower = 1000,
#'                        qupper = 1400,
#'                        sigma.dist = "lognormal",
#'                        sigma.lower = 1.5,
#'                        sigma.upper = 2.5,
#'                        distribution  = "Weibull")
#'  
#' # Create the prior distributions                      
#' prior3.bcage <- 
#'   make.prior(spec = prior.spec3, 
#'              number.in.prior = 3000)
#' 
#' 
#' prior.and.post3.bcage <-
#'   get.big.posterior(prior.spec3,
#'                     BearingCage.ld)
#' 
#' prior.and.post3.bcage$post[1:10,] 
#' 
#' prior.and.post3.bcage <- 
#'   make.small.posterior.object(prior.and.post3.bcage)
#'   
#' summarize.posterior.or.prior(prior.and.post3.bcage,
#'                              post.or.prior = "post",
#'                              task = "Marginals only",
#'                              marginal.on.sigma = T,
#'                              marginal.on.pos = F,
#'                              type.position = "Parameter",
#'                              newdata = "mu",
#'                              include.likelihood = T)
#' }
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
