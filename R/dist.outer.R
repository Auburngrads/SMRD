dist.outer <-
function (X, Y, dummy, FUN = "*", ...) 
{
    if (is.character(FUN)) 
        FUN <- get(envir = .frame0, FUN)
    if (is.array(X)) {
        nx <- dim(X)
        nmx <- dimnames(X)
    }
    else {
        nx <- length(X)
        nmx <- list(names(X))
    }
    if (is.array(Y)) {
        ny <- dim(Y)
        nmy <- dimnames(Y)
    }
    else {
        ny <- length(Y)
        nmy <- list(names(Y))
    }
    dims <- c(nx, ny)
    a <- matrix(X, length(X), length(Y))
    b <- matrix(Y, length(X), length(Y), byrow = T)
    dummy <- matrix(dummy, length(X), length(Y), byrow = T)
    ans <- FUN(a, dummy, b, ...)
    dim(ans) <- dims
    dn <- c(nmx, nmy)
    if (length(dn) == length(dims)) 
        dimnames(ans) <- dn
    ans
}
