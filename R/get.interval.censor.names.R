get.interval.censor.names <-
function (data.d) 
{
    the.interval.censor.names <- attr(data.d, "interval.censor.names")
    if (is.null(the.interval.censor.names)) {
        the.interval.censor.names <- data.d$interval.censor.names
        if (is.null(the.interval.censor.names)) {
            return(casefold(GetSMRDDefault("SMRD.IcName")))
        }
    }
    else {
        return(casefold(the.interval.censor.names))
    }
}
