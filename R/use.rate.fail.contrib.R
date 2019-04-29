use.rate.fail.contrib <-
function (yresp, failure.mode, use.rate.model, like.method = NULL, 
    eps = 0.003) 
{
    distributions <- paste(use.rate.model$distributions, collapse = "")
    switch(distributions, lognormallognormallognormallognormal = {
        the.contribution <- use.rate.fail.contrib.lognormal(yresp, 
            failure.mode, use.rate.model, like.method = like.method, 
            eps = eps)
    }, {
        stop(paste("Unknown dist = ", distributions))
    })
    return(the.contribution)
}
