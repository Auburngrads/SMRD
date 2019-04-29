rbvn <-
function (n, mu1, mu2, sigma1, sigma2, rho) 
{
    U <- rnorm(n)
    V <- rnorm(n)
    x1 <- mu1 + sigma1 * U
    x2 <- mu2 + sigma2 * rho * U + sigma2 * sqrt((1 - rho^2)) * 
        V
    return(cbind(x1, x2))
}
