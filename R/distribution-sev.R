#' The Smallest Extreme Value Distribution
#'
#' @export
#' @rdname SmallestExtremeValue

dsev <-
function (x, location = 0, scale = 1) 
{
    z <- (x - location)/scale
    return((1/scale) * exp(z - exp(z)))
}

#' @export
#' @rdname SmallestExtremeValue

psev <-
  function (q, location = 0, scale = 1) 
  {
    return(1 - ssev(q, location, scale))
  }

#' @export
#' @rdname SmallestExtremeValue

qsev <-
  function (p, location = 0, scale = 1) 
  {
    return(location + scale * logb(-logb(1 - p)))
  }

#' @export
#' @rdname SmallestExtremeValue

rsev <-
  function (n, location = 0, scale = 1) 
  {
    location + qsev(runif(n)) * scale
  }

#
#

ssev <-
  function (x, location = 0, scale = 1) 
  {
    return(exp(-exp((x - location)/scale)))
  }
