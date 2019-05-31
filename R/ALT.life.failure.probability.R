ALT.life.failure.probability <-
function (theta.hat, 
          time.vec, 
          distribution, 
          xuse.tran) 
{
    nter <- length(theta.hat) - 1
    mu <- theta.hat[1] + as.matrix(xuse.tran) %*% as.matrix(theta.hat[2:nter],
                                                            ncol = 1)
    if (is.logdist(distribution)) time.vec <- logb(time.vec)
    zvec <- (time.vec - c(mu)) / theta.hat[length(theta.hat)]
    answers <- wqmf.phibf(zvec, distribution)
    return(answers)
}
