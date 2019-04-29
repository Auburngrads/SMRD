multiple.get.location <-
function (mlest.out, stress) 
{
    frout <- matrix(c(1, matrix(stress, ncol = length(stress)), 
        0, 0, rep(0, length(stress)), 1), ncol = 2)
    gvec <- as.vector(mlest.out$theta.hat %*% frout)
    lsnames <- c("Location", "Scale")
    names(gvec) <- lsnames
    gmat <- t(frout) %*% mlest.out$vcv %*% frout
    dimnames(gmat) <- list(lsnames, lsnames)
    if (F) {
        the.list <- list(stress = stress, theta.hat = mlest.out$theta.hat, 
            gvec = gvec, gmat = gmat)
        print(the.list)
    }
    return(list(thetavec = gvec, vcv = gmat))
}
