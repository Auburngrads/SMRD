ADDT.life.quantile <-
function (p, 
          distribution, 
          FailLevel.tran, 
          xuse.tran, 
          theta.hat) 
{
    if (nrow(xuse.tran) > 1) {
        
        if (length(p) > 1 && length(p) != nrow(xuse.tran)) stop("length of p and x do not agree")
        if (length(p) == 1) p <- rep(p, nrow(xuse.tran))
        
    }
    
    beta2.names <- paste("beta", seq(2, ncol(xuse.tran) + 1),sep = "")
    theta.hat.names <- c("beta0", "beta1", beta2.names, "sigma")
    names(theta.hat) <- theta.hat.names
    Deg.diff <- theta.hat["beta0"] - FailLevel.tran
    beta2.vec <- theta.hat[beta2.names]
    answers <- rep(NA, length(p))
    beta.x <- as.matrix(xuse.tran) %*% as.matrix(theta.hat[beta2.names], 
                                                 ncol = 1)
    
    xfactor <- exp(-beta.x) / abs(theta.hat["beta1"])
    mu.quantile <- Deg.diff * xfactor
    sigma.quantile <- theta.hat[length(theta.hat)] * xfactor
    increasing <- theta.hat["beta1"] > 0
    
    if (increasing) {
        
        pnow <- 1 - p
        good.ones <- 1 - wqmf.phibf(-mu.quantile / sigma.quantile, distribution) < p
        answers[good.ones] <- -(c(mu.quantile) + quant(pnow[good.ones], distribution) * c(sigma.quantile))
        
    } else {
        
        pnow <- p
        good.ones <- as.numeric(wqmf.phibf(-mu.quantile/sigma.quantile, 
            distribution)) < p
        answers[good.ones] <- c(mu.quantile) + quant(pnow[good.ones], distribution) * c(sigma.quantile)
        
    }
    
    answers[!good.ones] <- 0
    return(answers)
    
}
