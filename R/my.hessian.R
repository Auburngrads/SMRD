my.hessian <-
function (f.fun, theta, delta = 0.001 * theta)
{
    identity <- diag(length(theta))
    hessian <- matrix(NA, nrow = length(theta), ncol = length(theta))
    xnames <- rep(NA, 4)
    fun.value <- f.fun(theta)
    
    for (i in 1:length(theta)) {
        
        deltai <- delta[i] * identity[, i]
        
        for (j in 1:i) {
            
            deltaj <- delta[j] * identity[, j]
            hessian[i, j] <- (f.fun(theta + deltai + deltaj) - 
                              f.fun(theta + deltaj - deltai) - 
                              f.fun(theta - deltaj + deltai) + 
                              f.fun(theta - deltaj - deltai)) / 
                             (4 * delta[i] * delta[j])
            hessian[j, i] <- hessian[i, j]
            
            if (map.SMRDDebugLevel() >= 6) {
                cat("\nIn my.hessian\n")
                cat("i=", i, "j=", j, "theta=", theta[i], theta[j],
                  "\ndelta=", delta[i], delta[j], "hess=", hessian[i,
                    j], "\n")
                assign(envir = .frame0, inherits = !TRUE,"xdebug", T)
                thetapp <- theta + deltai + deltaj
                xnames[1] <- "thetapp"
                thetapm <- theta + deltaj - deltai
                xnames[2] <- "thetapm"
                thetamp <- theta - deltaj + deltai
                xnames[3] <- "thetamp"
                thetamm <- theta - deltaj - deltai
                xnames[4] <- "thetamm"
                fvec <- c(f.fun(thetapp), 
                          f.fun(thetapm), 
                          f.fun(thetamp),
                          f.fun(thetamm))
                names(fvec) <- xnames
                print(fvec, digits = 9)
                assign(envir = .frame0, inherits = !TRUE,"xdebug", F)
                
            } else {
                
                assign(envir = .frame0, inherits = !TRUE,"xdebug", F)
            }
        }
    }
    return(hessian)
}
