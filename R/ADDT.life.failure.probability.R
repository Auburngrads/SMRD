ADDT.life.failure.probability <-
function (time.vec, 
          distribution, 
          FailLevel.tran, 
          xuse.tran, 
          theta.hat) 
{
    beta2.names <- paste("beta", seq(2, ncol(xuse.tran) + 1), sep = "")
    theta.hat.names <- c("beta0", "beta1", beta2.names, "sigma")
    names(theta.hat) <- theta.hat.names
    
    Deg.diff <- theta.hat["beta0"] - FailLevel.tran
    beta2.vec <- theta.hat[beta2.names]
    beta.x <- as.matrix(xuse.tran) %*% as.matrix(theta.hat[beta2.names], 
        ncol = 1)
    xfactor <- exp(-beta.x) / abs(theta.hat["beta1"])
    mu.quantile <- Deg.diff * xfactor
    sigma.quantile <- theta.hat[length(theta.hat)] * xfactor
    
    `if`(theta.hat["beta1"] > 0,
         answers <- 1 - wqmf.phibf((-time.vec - c(mu.quantile)) / c(sigma.quantile),distribution),
         answers <- wqmf.phibf((time.vec - c(mu.quantile)) / c(sigma.quantile),distribution))

    return(answers)
}
