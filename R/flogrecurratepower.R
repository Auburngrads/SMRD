flogrecurratepower <-
function (time, theta) 
{
    eta <- theta[1]
    beta <- theta[2]
    flogrecurratepower <- log(beta/eta) + (beta - 1) * log(time/eta)
    return(flogrecurratepower)
}
