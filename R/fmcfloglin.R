fmcfloglin <-
function (time, theta) 
{
    gamma0 <- theta[1]
    gamma1 <- theta[2]
    fmcfloglin <- (exp(gamma0)/gamma1) * (exp(gamma1 * time) - 
        1)
    return(fmcfloglin)
}
