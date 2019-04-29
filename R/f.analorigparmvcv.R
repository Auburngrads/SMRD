f.analorigparmvcv <-
function (vcv.gamma, theta, gamma1, gamma2.vec, tbar, xbar) 
{
    number.accelerators <- length(gamma2.vec)
    beta.x <- 0
    for (i in 1:number.accelerators) {
        beta.x <- beta.x + gamma2.vec[i] * xbar[i]
    }
    d.beta1.d.gamma2 <- rep(NA, length(number.accelerators))
    d.beta1.d.gamma1 <- exp(-beta.x)
    d.beta1.d.gamma2 <- -gamma1 * xbar * exp(-beta.x)
    d.beta2.d.gamma2 <- diag(number.accelerators)
    bottom.matrix <- cbind(matrix(0, nrow = number.accelerators, 
        ncol = 2), d.beta2.d.gamma2, matrix(0, nrow = number.accelerators, 
        ncol = 1))
    top.matrix <- cbind(matrix(c(1, 0, -tbar, d.beta1.d.gamma1), 
        ncol = 2), matrix(c(rep(0, number.accelerators + 1), 
        c(d.beta1.d.gamma2, 0)), nrow = 2, byrow = T))
    jacobian <- rbind(top.matrix, bottom.matrix, matrix(c(rep(0, 
        length(theta) - 1), 1), nrow = 1))
    if (map.SMRDDebugLevel() >= 4) {
        cat("jacobian\n")
        print(jacobian)
    }
    vcv.theta <- jacobian %*% vcv.gamma %*% t(jacobian)
    dimnames(vcv.theta) <- list(names(theta), names(theta))
    return(vcv.theta)
}
