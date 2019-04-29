xtab7 <-
function (pval, eta, n, xk, kprint = 0)
{
    number <- max(length(eta), length(n), length(xk))
    eta <- expand.vec(eta, number)
    n <- expand.vec(n, number)
    xk <- expand.vec(xk, number)
    zout <- .Fortran("sxtab7", eta = as.double(eta), n = as.integer(n),
        xk = as.double(xk), number = as.integer(number), pval = double(pval),
        ier = integer(number), kprint = as.integer(kprint))
    return(zout$xk)
}
