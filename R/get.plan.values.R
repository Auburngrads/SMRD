#' Get planning value for a reliability test
#'
#' @param distribution 
#' @param beta 
#' @param sigma 
#' @param prob 
#' @param time 
#' @param time.units 
#'
#' @return A list of reliability test plan values
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'  plan.values1 <- get.plan.values("Weibull", 
#'                                  beta = 2, 
#'                                  prob = .1, 
#'                                  time = 100, 
#'                                  time.units = "Hours")
#'                                  
#' life.test.simulation(plan.values1, 
#'                      n = 50,
#'                      censor.time = 120, 
#'                      number.detail = 5, 
#'                      quantile.mark = 0.2) 
#'                      
#' plan.values2 <- get.plan.values("Lognormal", 
#'                                 sigma = 0.5,
#'                                 prob = 0.1, 
#'                                 time = 100, 
#'                                 time.units = "Hours")
#' 
#' summary(plan.values2)
#' plot(plan.values2)
#' 
#' plot(plan.values2, censor.time=1000, grids=F)
#' 
#' life.test.simulation(plan.values2, 
#'                      n = 50,
#'                      censor.time = 1000, 
#'                      quantile.mark = .1)
#'
#' plan.values3 <- get.plan.values("Weibull",
#'                                 prob = c(.2,.12),
#'                                 time = c(1000,500), 
#'                                 time.units = "Hours")
#' 
#' plan.values4 <- get.plan.values("Weibull",
#'                                 prob = c(.05,.15),
#'                                 time = c(40000,100000),
#'                                 time.units = "Hours")
#' 
#' summary(plan.values3)
#' plot(plan.values3)
#' 
#' 
#' life.test.simulation(plan.values3, 
#'                      n = 50, 
#'                      censor.time = 1000, 
#'                      quantile.mark = 0.1)
#' 
#' #compare the simulated value with the large-sample approx below
#' 
#' asd.quant(plan.values3, 
#'           n=50, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)
#' 
#' #compare:
#' 
#' asd.quant(plan.values3,
#'           n=50, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(50)
#' 
#' asd.quant(plan.values3,
#'           n=500, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(500)
#' 
#' asd.quant(plan.values3,
#'           n=5000, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(5000)
#' 
#' }
#' @seealso code{link{life.test.simulation}}
#'          code{link{asd.quant}}
get.plan.values <-
function (distribution, beta, sigma, prob, time, time.units = "Time")
{
  
  if (missing(distribution))          stop("Distribution is missing and must be specified")
  if (missing(prob) || missing(time)) stop("Must specify values for both prob and time")
  if (length(prob)  != length(time))  stop("Length of prob and time are not equal")
  
  if (length(prob)==1) {
  
  if  (is.even(numdist(distribution))) time <- logb(time)
  
    if(missing(beta)) {
      
       if (missing(sigma)) stop("Must specify either sigma or beta")
       beta <- 1/sigma
       
  } else {
    
        `if`(missing(sigma),
             sigma <- 1/beta,
             stop("Cannot specify both sigma and beta"))
        
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
  rlist <- list(distribution = distribution, 
                mu = mu, 
                sigma = sigma,
                zc1 = zc1, 
                beta = beta, 
                probs = prob, 
                times = time, 
                eta = eta,
                time.units = time.units)
  
  oldClass(rlist) <- "plan.values"
  return(rlist)

  } else {
  
    if (!missing(beta) || !missing(sigma)) stop("Multiple time/prob variables specified along with slope value (beta/sigma)")
      
  get.plan.values.from.two.points(distribution = distribution, 
                                  times = time, 
                                  probs = prob,
                                  time.units = time.units)
    
  }
    
    
}
