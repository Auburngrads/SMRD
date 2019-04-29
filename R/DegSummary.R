DegSummary <-
function (rmd.object) 
{
    Fixed.vcv <- rmd.object$varFix
    Var.vcv <- rmd.object$apVar
    parameter.names <- c("mu.beta0", "mu.beta1", "sigma.beta0", 
        "sigma.beta1", "rho", "sigma.eps")
    if (is.matrix(Var.vcv)) {
        top <- cbind(Fixed.vcv, matrix(0, ncol = ncol(Var.vcv), 
            nrow = nrow(Fixed.vcv)))
        bottom <- cbind(matrix(0, ncol = ncol(Fixed.vcv), nrow = nrow(Var.vcv)), 
            Var.vcv)
        vcv <- rbind(top, bottom)
        dimnames(vcv) <- list(parameter.names, parameter.names)
        the.det <- prod(eigen(vcv)$values)
    }
    else {
        vcv <- "Non-positive definite approximate variance-covariance"
        the.det <- NA
    }
    Random.vcv <- as.matrix(rmd.object$modelStruct[[1]][[1]]) * 
        (rmd.object$sigma^2)
    std.deviations <- sqrt(diag(Random.vcv))
    correlation <- Random.vcv[1, 2]/(std.deviations[1] * std.deviations[2])
    parameters <- c(fixed.effects(rmd.object), std.deviations, 
        correlation, rmd.object$sigma)
    names(parameters) <- parameter.names
    return(list(parameters = parameters, vcv = vcv, log.det = log(the.det)))
}
