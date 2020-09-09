#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param debug1 
#' @param theta.start 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1, 
#'                                 censor.column = 3,
#'                                 time.units = "Kilometers")
#'                                 
#' SA.weibull.gmle <- ls2.mle(ShockAbsorber.ld, 
#'                           distribution = "Weibull")
#'                           
#' BearingCage.ld <- frame.to.ld(bearingcage,
#'                               response.column = 1, 
#'                               censor.column = 2, 
#'                               case.weight.column = 3,
#'                               time.units = "Hours")
#' 
#' summary(BearingCage.ld)
#' 
#' ls2.mle(BearingCage.ld, 
#'         distribution ="Weibull", 
#'         theta.start = c(9,.4))
#' }
ls2.mle <-
function (data.ld, 
          distribution = "weibull",
          debug1 = F, 
          theta.start = NULL) 
{
    options(digits = 5)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    special.stuff <- list(1)
    orig.param.names <- c("location", "scale")
    t.param.names <- c("location", "scale")
    model <- list(distribution = distribution, form = NULL)
    model$orig.param.names <- orig.param.names
    model$t.param.names <- t.param.names
    
    if (is.null(theta.start)) {
      
        theta.start <- mlest(data.ld, distribution)$theta.hat
        
    }
    
    gmle.out <- gmle(log.like = ls2.log.like,
                     data.ld = data.ld,
                     theta.start = theta.start,
                     model = model,
                     special.stuff = special.stuff,
                     t.param.names = t.param.names)
    
    theta.hat <- gmle.out$est.out$x
    mu <- theta.hat[1]
    sigma <- exp(theta.hat[2])
    parameters <- list(mu = mu, sigma = sigma)
    gmle.out$parameters <- parameters
    return(gmle.out)
    
}
