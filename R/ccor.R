ccor <-
function (vcv) 
{
    if (length(vcv) == 1) {
        xx <- as.matrix(1)
    }
    else {
        xx <- diag(1/sqrt(abs(diag(vcv)))) %*% vcv %*% diag(1/sqrt(abs(diag(vcv))))
    }
    dimnames(xx) <- dimnames(vcv)
    return(xx)
}
