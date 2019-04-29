get.vcv.names <-
function (param.names) 
{
    k <- 0
    p <- length(param.names)
    vcv.names <- rep(NA, (p * (p + 1))/2)
    for (i in 1:p) {
        for (j in i:p) {
            k <- k + 1
            vcv.names[k] <- paste(param.names[i], param.names[j], 
                sep = ",")
        }
    }
    return(vcv.names)
}
