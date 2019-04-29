use.rate.fail.contrib.lognormal <-
function (yresp, failure.mode, use.rate.model, eps = 0.005, like.method = NULL) 
{
    if (is.null(like.method)) {
        warning("Likelihood method not specified in UR model spec---setting to delta")
        like.method <- "delta"
    }
    the.contribution <- switch(like.method, delta = {
        use.rate.fail.contrib.lognormal.delta(yresp, failure.mode, 
            use.rate.model, eps = eps)
    }, density = {
        the.contribution <- use.rate.fail.contrib.lognormal.density(yresp, 
            failure.mode, use.rate.model, eps = eps)
    }, {
        stop(paste(like.method, "is not a recognized like.method"))
    })
    return(the.contribution)
}
