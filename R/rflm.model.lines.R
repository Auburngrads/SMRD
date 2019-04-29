rflm.model.lines <-
function (gmle.out, quant.lines = c(0.05, 0.5, 0.95), ylim = c(NA, 
    NA), xlim = c(NA, NA), pts = 100) 
{
    theta <- c(gmle.out$origparam["beta0"], gmle.out$origparam["beta1"], 
        gmle.out$origparam["sigma"], gmle.out$origparam["mu.gamma"], 
        gmle.out$origparam["sigma.gamma"])
    data.ld <- gmle.out$data.ld
    xval <- xmat(data.ld)
    if (is.na(xlim[1])) 
        xlim[1] <- min(xval)
    if (is.na(xlim[2])) 
        xlim[2] <- max(xval)
    yval <-Response(data.ld)
    beta0 <- theta[1]
    beta1 <- theta[2]
    sigma <- theta[3]
    mugamma <- theta[4]
    sdgamma <- theta[5]
    stresslab <- get.xlabel(data.ld)
    if (gmle.out$model$fl.dist == 1) 
        fl.dist <- psev
    if (gmle.out$model$fl.dist == 2) 
        fl.dist <- pnorm
    if (gmle.out$model$fl.dist == 3) 
        fl.dist <- plogis
    incre <- (logb(xlim[2]) - logb(xlim[1]))/pts
    tot <- pts:0
    x <- exp(logb(xlim[1]) + incre * (pts - tot))
    yquant.mat <- matrix(NA, ncol = length(quant.lines), nrow = pts + 
        1)
    for (i in 1:(pts + 1)) {
        if (i > 1) 
            limits <- c(yquant.mat[i - 1, 1], yquant.mat[i - 
                1, 1] + 2)
        else limits <- logb(range(yval))
        for (j in 1:length(quant.lines)) {
            check <- fl.dist(logb(x[i]), mugamma, sdgamma)
            if (check > quant.lines[j]) {
                yquant.mat[i, j] <- rflm.quan(quant.lines[j], 
                  gmle.out$model$cond.dist, gmle.out$model$fl.dist, 
                  beta0, beta1, x[i], sigma, mugamma, sdgamma, 
                  bds = limits)
            }
            limits <- c(yquant.mat[i, j], yquant.mat[i, j] + 
                2)
        }
    }
    return(list(xvec = x, yquant.mat = (yquant.mat)))
}
