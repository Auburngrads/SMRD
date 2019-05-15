#' Return asd quantity
#'
#' @param plan.values 
#' @param n 
#' @param censor.time 
#' @param quantile.mark 
#'
#' @return variance
#' @export
#'
#' @examples
#' \dontrun{
#' plan.values3 <- get.plan.values("Weibull",
#'                                 prob = c(.2,.12),
#'                                 time = c(1000,500), 
#'                                 time.units = "Hours")
#' 
#' asd.quant(plan.values3, 
#'           n = 50, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)
#' 
#' #compare:
#' 
#' asd.quant(plan.values3,
#'           n = 50, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(50)
#' 
#' asd.quant(plan.values3,
#'           n = 500, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(500)
#' 
#' asd.quant(plan.values3,
#'           n = 5000, 
#'           censor.time = 1000, 
#'           quantile.mark = 0.1)*sqrt(5000)
#' 
#' }
#' @seealso code{link{get.plan.values}}
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
    
    zout <- VAVAR(as.integer(numdist(distribution)),
                  as.integer(vlength),
                  as.double(zc),
                  as.double(ze),
                  double(vlength))
    
    return(sqrt(zout$avar / n) * plan.values$sigma)
}
