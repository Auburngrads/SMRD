xtab1 <-
function (pval, gamma, n, kprint = 0) 
{
    number <- max(length(pval), length(gamma), length(n))
    pval <- expand.vec(pval, number)
    gamma <- expand.vec(gamma, number)
    n <- expand.vec(n, number)
    zout <- .Fortran("sxtab1", pval = as.double(pval), gamma = as.double(gamma), 
        n = as.integer(n), number = as.integer(number), xk = double(number), 
        ier = integer(number), kprint = as.integer(kprint))
    return(zout$xk)
}
