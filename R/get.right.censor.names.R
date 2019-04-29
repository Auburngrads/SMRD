get.right.censor.names <-
function (data.d)
{
    the.right.censor.names <- attr(data.d, "right.censor.names")
    if (is.null(the.right.censor.names)) {
        the.right.censor.names <- data.d$right.censor.names
        if (is.null(the.right.censor.names)) {
            return(casefold(GetSMRDDefault("SMRD.RcName")))
        }
  } else {
        return(casefold(the.right.censor.names))
    }
}
