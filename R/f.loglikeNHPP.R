f.loglikeNHPP <-
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
    the.log.like <- TestLike(data.rdu, theta.origparam, form)
    if (map.SMRDDebugLevel() >= 4 || iter.count%%10 == 0) {
        p.names <- names(theta.origparam)
        cat("loglikeNHPP Iter = ", iter.count, "Loglikelihood =",
            format(the.log.like), paste(p.names, format(theta.origparam,
                digits = 4), sep = " = "), "\n")
    }
    return(-the.log.like)
}
