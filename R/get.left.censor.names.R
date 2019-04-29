get.left.censor.names <-
function (data.d) 
{
    the.left.censor.names <- attr(data.d, "left.censor.names")
    if (is.null(the.left.censor.names)) {
        the.left.censor.names <- data.d$left.censor.names
        if (is.null(the.left.censor.names)) {
            return(casefold(GetSMRDDefault("SMRD.LcName")))
        }
    }
    else {
        return(casefold(the.left.censor.names))
    }
}
