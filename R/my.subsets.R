my.subsets <-
function (a, sizes, log = FALSE) 
{
    if (a == sizes) 
        return(list(seq(1:a)))
    lnames <- NULL
    if (is.array(a)) {
        nc <- length(dim(a))
        if (!is.null(names(dimnames(a)))) 
            seqq <- c("overall", names(dimnames(a)))
        else seqq <- 0:nc
    }
    else if (length(a) > 1) {
        nc <- length(a)
        seqq <- c(NA, a)
    }
    else {
        nc <- as.integer(a)
        seqq <- 0:nc
    }
    n <- 2^nc
    mat <- mod(0:(n - 1))[, nc:1]
    isizes <- apply(mat, 1, sum)
    iperm <- order(isizes, 1:n)
    if (!missing(sizes)) 
        iperm <- iperm[!is.na(match(isizes[iperm], sizes))]
    mat <- mat[iperm, ]
    if (log) 
        return(mat)
    mat <- cbind(c(T, rep(F, n - 1))[iperm], mat)
    ll <- NULL
    for (i in 1:nrow(mat)) ll <- c(ll, list(seqq[mat[i, ]]))
    ll
}
