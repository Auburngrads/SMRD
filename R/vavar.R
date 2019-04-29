vavar <-
function (mu, sigma, tc, pe, distribution) 
{
    lm <- length(mu)
    ls <- length(sigma)
    lt <- length(tc)
    lp <- length(pe)
    vlength <- max(lm, ls, lt, lp)
    mu <- expand.vec(mu, vlength)
    sigma <- expand.vec(sigma, vlength)
    tc <- expand.vec(tc, vlength)
    pe <- expand.vec(pe, vlength)
    ze <- quant(pe, distribution)
    zc <- (logb(tc) - mu)/sigma
    switch(generic.distribution(distribution), sev = idist <- 1, 
        lev = idist <- 2, normal = idist <- 3, logistic = idist <- 4, 
        stop("Distribution must be sev, lev, normal, or logistic"))
    zout <- .Fortran("vavar", as.integer(idist), as.integer(vlength), 
        as.double(zc), as.double(ze), ans = double(vlength))
    return(zout$ans)
}
