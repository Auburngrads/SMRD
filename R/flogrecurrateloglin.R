flogrecurrateloglin <-
function (time, theta) 
{
    gamma0 <- theta[1]
    gamma1 <- theta[2]
    flogrecurrateloglin <- gamma0 + gamma1 * time
    return(flogrecurrateloglin)
}
