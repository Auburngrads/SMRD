#' The Largest Extreme Value Distribution
#'
#' @export
#' @rdname LargestExtremeValue

dlev <-
function (x, location = 0, scale = 1) 
{
    zvec <- (x - location)/scale
    return((1/scale) * exp(-zvec - exp(-zvec)))
}

#' @export
#' @rdname LargestExtremeValue

plev <-
  function (q, location = 0, scale = 1) 
  {
    return(exp(-exp(-(q - location)/scale)))
  }

#' @export
#' @rdname LargestExtremeValue

qlev <-
  function (p, location = 0, scale = 1) 
  {
    return(location - scale * logb(-logb(p)))
  }

#
#

slev <-
  function (x, loc = 0, scale = 1) 
  {
    return(1 - exp(-exp(-(x - loc)/scale)))
  }
