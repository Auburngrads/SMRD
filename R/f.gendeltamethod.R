f.gendeltamethod <-
function (vcv.theta, 
          theta, 
          gfun,...) 
{
    gtheta <- gfun(theta, ...)
    sevec <- rep(NA, length = length(gtheta))
    for (j in 1:length(gtheta)) {
        gradnow <- as.matrix(my.gradient(gfun, theta, function.element = j, 
            delta = 1e-05 * theta, ...))
        sevec[j] <- sqrt(t(gradnow) %*% vcv.theta %*% gradnow)
        if (map.SMRDDebugLevel() >= 5) {
            cat("the grad in f.gendeltamethod\n")
            print(gradnow)
        }
    }
    return(list(vec = gtheta, se = sevec))
}
