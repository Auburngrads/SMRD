ALT.life.quantile <-
function (theta.hat, p, distribution, xuse.tran) 
{
    if (nrow(xuse.tran) > 1) {
        if (length(p) > 1 && length(p) != nrow(xuse.tran)) 
            stop("length of p and x do not agree")
        if (length(p) == 1) 
            p <- rep(p, nrow(xuse.tran))
    }
    nter <- length(theta.hat) - 1
    answers <- theta.hat[1] + as.matrix(xuse.tran) %*% as.matrix(theta.hat[2:nter], 
        ncol = 1) + quant(p, distribution) * theta.hat[length(theta.hat)]
    if (is.logdist(distribution)) 
        answers <- exp(answers)
    return(answers)
}
