fmcfpower <-
function (time, theta) 
{
    eta <- theta[1]
    beta <- theta[2]
    fmcfpower <- (time/eta)^beta
    return(fmcfpower)
}
