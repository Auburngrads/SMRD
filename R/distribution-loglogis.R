#' The Loglogistic Distribution
#'
#' @export
#' @rdname Loglogistic

dloglogis <-
function (x, locationlog = 0, scalelog = 1) 
{
    return((1/(x * scalelog)) * dlogis((logb(x) - locationlog)/scalelog))
}

#' @export
#' @rdname Loglogistic

ploglogis <-
  function (q, locationlog = 0, scalelog = 1) 
  {
    return(plogis((logb(q) - locationlog)/scalelog))
  }

#' @export
#' @rdname Loglogistic

qloglogis <-
  function (p, locationlog = 0, scalelog = 1) 
  {
    return(exp(locationlog + scalelog * qlogis(p)))
  }

#'
#'

