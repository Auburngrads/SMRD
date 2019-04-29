f.loglikeNHPPvec <-
function (thetain) 
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    data.rdu <- get(envir = .frame0,  "data.ld")
   debug1<- get(envir = .frame0,  "debug1")
    model <- get(envir = .frame0,  "model")
    form <- model$form
    f.origparam <- model$f.origparam
    theta.origparam <- f.origparam(thetain, model)
    if (iter.count < 4) 
        browser()
    the.log.like <- loglikeNHPPvec(data.rdu, theta.origparam, 
        form)
    cat(iter.count, format(the.log.like), "theta1=", format(theta.origparam[1]), 
        format(theta.origparam[2]), "\n")
    return(-the.log.like)
}
