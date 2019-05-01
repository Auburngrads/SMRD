#' The Birnbaum Sanders Distribution
#'
#' @export
#' @rdname Birnbaum-Sanders

dbisa <-
function (x, shape, scale = 1) 
{
    return((1/(x)) * dlbisa(logb(x/scale), shape))
}

#
#

dlbisa <-
  function (z, shape) 
  {
    z1 <- (exp(z/2) - exp(-z/2))/shape
    return((exp(z/2) + exp(-z/2))/(2 * shape) * dnorm(z1))
  }

#' @export
#' @rdname Birnbaum-Sanders

pbisa <-
  function (q, shape, scale = 1) 
  {
    return(plbisa(logb(q/scale), shape))
  }

#
#

plbisa <-
  function (z, shape) 
  {
    z1 <- (exp(z/2) - exp(-z/2))/shape
    return(pnorm(z1))
  }

#' @export
#' @rdname Birnbaum-Sanders

qbisa <-
  function (p, shape, scale = 1) 
  {
    return(scale * ((shape * qnorm(p) + sqrt(4 + (shape * 
                                                       qnorm(p))^2))/2)^2)
  }

#
#

sbisa <- function (x, shape) { 1 - pbisa(x, shape) }
