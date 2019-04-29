get.failure.censor.names <-
function (data.d) 
{
    the.failure.censor.names <- attr(data.d, "failure.censor.names")
    if (is.null(the.failure.censor.names)) {
        the.failure.censor.names <- data.d$failure.censor.names
        if (is.null(the.failure.censor.names)) {
            return(casefold(GetSMRDDefault("SMRD.FailName")))
        }
    }
    else {
        return(casefold(the.failure.censor.names))
    }
}
