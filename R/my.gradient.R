my.gradient <-
function (f.fun, 
          theta, 
          function.element = 1, 
          delta = 1e-06 * theta, ...) 
{
    delta.eq.zero <- delta == 0
    if (any(delta.eq.zero)) delta[delta.eq.zero] <- 1e-05
    gradient <- rep(NA, length(theta))
    
    for (i in 1:length(theta)) {
        
        deltai <- rep(0, length(theta))
        deltai[i] <- delta[i]
        gradient[i] <- (f.fun(theta + deltai, ...)[function.element] - 
                        f.fun(theta - deltai, ...)[function.element]) / 
                       (2 * delta[i])
    }
    names(gradient) <- names(theta)
    return(gradient)
}
