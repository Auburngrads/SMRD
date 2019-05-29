ADDT.degradation.quantile <-
function (p, distribution, xuse.tran, time.tran, theta.hat) 
{
    if (F) {
        if (nrow(xuse.tran) > 1) {
            if (length(p) > 1 && length(p) != nrow(xuse.tran)) 
                stop("length of p and x do not agree")
            if (length(p) == 1) 
                p <- rep(p, nrow(xuse.tran))
        }
    }
    beta2.names <- paste("beta", seq(2, ncol(xuse.tran) + 1), 
        sep = "")
    theta.hat.names <- c("beta0", "beta1", beta2.names, "sigma")
    names(theta.hat) <- theta.hat.names
    beta2.vec <- theta.hat[beta2.names]
    beta.x <- as.matrix(xuse.tran) %*% as.matrix(theta.hat[beta2.names], 
        ncol = 1)
    slope <- theta.hat["beta1"] * exp(beta.x)
    intercept <- theta.hat["beta0"]
    answers <- intercept + as.vector(slope) * time.tran + quant(p, distribution) * 
        theta.hat["sigma"]
    return(answers)
}
