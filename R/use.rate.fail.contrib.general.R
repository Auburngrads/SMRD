use.rate.fail.contrib.general <-
function (yresp, failure.mode, use.rate.model, eps = 0.005) 
{
    distributions <- use.rate.model$distributions
    switch(paste(distributions, ""), lognormallognormal = {
        the.contribution <- use.rate.fail.contrib.lognormal(yresp, 
            failure.mode, use.rate.model)
    }, {
        stop("Unknown dist")
    })
    return(the.contribution)
}
