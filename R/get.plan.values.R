get.plan.values <-
function (distribution, beta, sigma, prob, time, time.units = "Time")
{
  
  if (missing(distribution))          stop("Distribution is missing and must be specified")
  if (missing(prob) || missing(time)) stop("Must specify values for both prob and time")
  if (length(prob)  != length(time))  stop("Length of prob and time are not equal")
  
  if (length(prob)==1) {
  
  if  (is.even(numdist(distribution)))
        time <- logb(time)
    if (missing(beta)) {
        if (missing(sigma)) {
            stop("Must specify either sigma or beta")
        }
        beta <- 1/sigma
  } else {
        if (missing(sigma)) {
            sigma <- 1/beta
      } else {
            stop("Cannot specify both sigma and beta")
        }
  }
  
  zc1 <- quant(prob, distribution)
  mu <- time - zc1 * sigma
  if (is.logdist(distribution)) {
    time <- exp(time)
    eta <- exp(mu)
  } else {
    eta <- NA
    beta <- NA
  }
  rlist <- list(distribution = distribution, mu = mu, sigma = sigma,
                zc1 = zc1, beta = beta, probs = prob, times = time, eta = eta,
                time.units = time.units)
  oldClass(rlist) <- "plan.values"
  return(rlist)

  } else {
  
    if (!missing(beta) || !missing(sigma))
      stop("Multiple time/prob variables specified along with slope value (beta/sigma)")
      
  get.plan.values.from.two.points(distribution = distribution, times = time, probs = prob,
                                  time.units = time.units)
    
  }
    
    
}
