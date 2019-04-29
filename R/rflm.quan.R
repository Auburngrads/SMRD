rflm.quan <-
function (alpha, ndist1, ndist2, beta0 = 10, beta1 = -2, stress = 20, 
    sigma = 0.5, mugamma = 3.2, sdgamma = 0.08, bds = c(2, 10)) 
{
    if (is.na(bds[1])) 
        bds <- c(5, 8)
    if (ndist2 == 1) 
        check <- psev(logb(stress), mugamma, sdgamma)
    if (ndist2 == 2) 
        check <- pnorm(logb(stress), mugamma, sdgamma)
    if (ndist2 == 3) 
        check <- plogis(logb(stress), mugamma, sdgamma)
    if (check > alpha) {
        quan <- beta0.2.quan(ndist1, ndist2, alpha, bds, beta0, 
            beta1, stress, sigma, mugamma, sdgamma)
        return(quan)
    }
    else {
        return(NA)
        print("The quantile is higher than proportion failing.")
        print(paste("alpha =", alpha))
        print(paste("propn failing =", check))
        stop()
    }
}
