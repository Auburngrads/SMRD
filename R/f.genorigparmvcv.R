f.genorigparmvcv <-
function (vcv.theta, theta, gfun, ...) 
{
    gtheta <- gfun(theta, ...)
    jacobi <- matrix(NA, ncol = length(gtheta), nrow = length(theta))
    for (j in 1:length(gtheta)) {
        jacobi[, j] <- my.gradient(gfun, theta, function.element = j, 
            ...)
    }
    vcv.gtheta <- t(jacobi) %*% vcv.theta %*% jacobi
    dimnames(vcv.gtheta) <- list(names(gtheta), names(gtheta))
    return(list(vec = gtheta, vcv = vcv.gtheta))
}
