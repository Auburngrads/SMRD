get.sub.model.dest.degrad.mle.out <-
function (x, stresses,...)
{
    transformation.x <- fix.inverse.relationship(x$model$transformation.x)
    trans.stresses <- as.matrix(stresses)
    for (i in 1:ncol(trans.stresses)) {
        trans.stresses[, i] <- f.relationship(trans.stresses[,
            i], subscript.relationship(transformation.x, i))
    }
    origparam <- x$origparam
    beta2.names <- paste("beta", seq(2, ncol(trans.stresses) +
        1), sep = "")
    beta2.vec <- origparam[beta2.names]
    x.beta <- trans.stresses %*% beta2.vec
    return(list(x.value = stresses, intercept = rep(origparam["beta0"],
        nrow(trans.stresses)), slope = origparam["beta1"] * exp(x.beta)))
}
