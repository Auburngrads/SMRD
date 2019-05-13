#' Title
#'
#' @param plan.values 
#' @param n 
#' @param proportion.failing 
#' @param quantiles 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'  plan.values3 <- get.plan.values("Weibull",
#'                                  prob = c(.2,.12),
#'                                  time = c(1000,500), 
#'                                  time.units = "Hours")
#'                                
#'  asym.test.plan.properties(plan.values3, 
#'                            n = 50, 
#'                            proportion.failing = 0.1)
#' 
#' }
asym.test.plan.properties <-
function (plan.values, n, proportion.failing, quantiles = c(0.1, 
    0.3, 0.5, 0.632)) 
{
    distribution <- basic.distribution(plan.values$distribution)
    mu <- plan.values$mu
    sigma <- plan.values$sigma
    std.quant.of.int <- quant(as.numeric(quantiles), distribution)
    std.log.censor.time <- quant(as.numeric(proportion.failing), 
        distribution)
    var.out <- ftavarvec(distribution, std.log.censor.time)
    sfactor <- sigma^2/n
    sdvec <- sqrt(sfactor * c(var.out$v11 + 2 * std.quant.of.int * 
        var.out$v12 + (std.quant.of.int)^2 * var.out$v22))
    answer <- cbind(quantile = quantiles, sd = sdvec)
    return(answer)
}
