FixTruncFilter <-
function (data.ld)
{
    the.censor.codes <- censor.codes(data.ld)
    the.truncation.codes <- truncation.codes(data.ld)
    leftleft <- the.censor.codes == 3 & the.truncation.codes ==
        3
    rightright <- the.censor.codes == 2 & the.truncation.codes ==
        2
    if (any(leftleft) || any(rightright)) {
        theResponse <-Response(data.ld)
        if (ncol(Response(data.ld)) == 1) {
           Response(data.ld) <- cbind(theResponse, theResponse)
            theResponse <-Response(data.ld)
        }
        ty <- truncation.response(data.ld)
        if (any(leftleft)) {
            theResponse[leftleft, 1] <- ty[leftleft, 1]
            the.censor.codes[leftleft] <- 4
        }
        if (any(rightright)) {
            theResponse[rightright, 2] <- ty[rightright, 1]
            the.censor.codes[rightright] <- 4
        }
        censor.codes(data.ld) <- the.censor.codes
       Response(data.ld) <- theResponse
    }
    return(data.ld)
}
