get.location <-
function (mlest.out, stress) 
{
    if (mlest.out$relationship == "class") {
        frout <- matrix(c(1, f.relationship(stress, mlest.out$relationship), 
            0, 0, 0, 1), ncol = 2)
    }
    else {
        frout <- matrix(c(1, f.relationship(stress, mlest.out$relationship), 
            0, 0, 0, 1), ncol = 2)
    }
    gvec <- mlest.out$theta.hat %*% frout
    gmat <- t(frout) %*% mlest.out$vcv %*% frout
    return(list(thetavec = gvec, vcv = gmat))
}
