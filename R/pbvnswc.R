pbvnswc <-
function (ah, ak, mu1, mu2, sd1, sd2, rho, numsig = 7) 
{
    infinity1 <- mu1 + numsig * sd1
    infinity2 <- mu2 + numsig * sd2
    up <- bvn(mu1, mu2, sd1, sd2, rho, ah, -infinity2)
    right <- bvn(mu1, mu2, sd1, sd2, rho, -infinity1, ak)
    ne <- bvn(mu1, mu2, sd1, sd2, rho, ah, ak)
    return(up - right + ne)
}
