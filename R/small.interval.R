#' Title
#'
#' @param data.ld 
#' @param delta 
#' @param tlog 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Fan.ld <- frame.to.ld(fan,
#'                       response.column = 1, 
#'                       censor.column = 2, 
#'                       case.weight.column = 3,
#'                       time.units = "Hours")
#'                     
#' gets.mle(small.interval(Fan.ld, delta = 0.01),
#'          distribution = "sev")
#' 
#' gets.mle(small.interval(Fan.ld, delta = 0.01),
#'          distribution = "normal")
#'
#' }
small.interval <-
function (data.ld, delta = 1e-05, tlog = T)
{
    the.censor.codes <- censor.codes(data.ld)
    theResponse <-Response(data.ld)
    if (ncol(theResponse) == 1)
        y <- cbind(theResponse, theResponse)
    else y <- theResponse
    exact <- the.censor.codes == 1
    if (tlog) {
        y[exact, 1] <- theResponse[exact, 1]/(1 + delta)
        y[exact, 2] <- theResponse[exact, ncol(theResponse)] *
            (1 + delta)
  } else {
        y[exact, 1] <- theResponse[exact, 1] - delta
        y[exact, 2] <- theResponse[exact, ncol(theResponse)] +
            delta
    }
    the.censor.codes[exact] <- 4
   Response(data.ld = data.ld) <- y
    censor.codes(data.ld = data.ld) <- the.censor.codes
    return(data.ld)
}
