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
