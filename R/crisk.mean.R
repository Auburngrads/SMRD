crisk.mean <-
function (mu1, sig1, mu2, sig2, distribution = "Weibull", mode = "b", 
    smallq = 1e-04, bigq = 1 - smallq) 
{
    switch(mode, b = {
        FUN <- function(t, mu1, sig1, mu2, sig2, distribution) {
            z1 <- (logb(t) - mu1)/sig1
            z2 <- (logb(t) - mu2)/sig2
            return((1 - wqmf.phibf(z1, distribution)) * (1 - 
                wqmf.phibf(z2, distribution)))
        }
    }, m1 = {
        FUN <- function(t, mu1, sig1, mu2, sig2, distribution) {
            z1 <- (logb(t) - mu1)/sig1
            return((1 - wqmf.phibf(z1, distribution)))
        }
    }, m2 = {
        FUN <- function(t, mu1, sig1, mu2, sig2, distribution) {
            z2 <- (logb(t) - mu2)/sig2
            return((1 - wqmf.phibf(z2, distribution)))
        }
    })
    lower <- exp(min(mu1 + sig1 * quant(smallq, distribution), 
        mu2 + sig2 * quant(smallq, distribution)))
    upper <- exp(min(mu1 + sig1 * quant(bigq, distribution), 
        mu2 + sig2 * quant(bigq, distribution)))
    result <- integrate(FUN, lower, upper, mu1 = mu1, sig1 = sig1, 
        mu2 = mu2, sig2 = sig2, distribution = distribution)
    return(result$aux$area)
}
