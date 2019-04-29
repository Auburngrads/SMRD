lsinf <-
function (z, censor.type, distribution, force = F,debug1= F)
{
    if (!is.numeric(z))
        stop("z must be numeric")
    if (!is.character(censor.type))
        stop("censor.type must be character")
    if (!is.character(distribution))
        stop("distribution must be character string")
    switch(generic.distribution(distribution), weibull = idist <- 2, sev = idist <- 1,
        frechet = , lev = idist <- 2, lognormal = , normal = idist <- 3,
        loglogistic = , logistic = idist <- 4, stop("Distribution must be sev, lev, normal, or logistic"))
    switch(censor.type, uncensored = icode <- 1, right = icode <- 2,
        left = icode <- 3, stop("censor.type must be uncensored, left, or right"))
    nrows <- length(z)
    if (debug1)
        browser()
    zout <- .Fortran("slsinf", as.integer(idist), as.integer(icode),
        as.double(z), as.double(z), f11 = double(nrows), f12 = double(nrows),
        f22 = double(nrows), as.integer(nrows), ifault = integer(1),
        irow = integer(1))
    
    if(length(z)==1) {
    zmat <- matrix(c(zout$f11,zout$f12,zout$f12,zout$f22), nrow = 2, byrow = TRUE)
    colnames(zmat) <- c("f_i1", "f_i2")
    rownames(zmat) <- c("f_1j", "f_2j")
} else zmat <- NULL
    
    return(list(f11 = zout$f11, f12 = zout$f12, f22 = zout$f22, matrix = zmat))
}
