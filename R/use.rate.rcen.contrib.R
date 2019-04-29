use.rate.rcen.contrib <-
function (yresp, use.rate.model) 
{
    distributions <- paste(use.rate.model$distributions, collapse = "")
    switch(distributions, lognormallognormallognormallognormal = {
        the.contribution <- use.rate.rcen.contrib.lognormal(yresp, 
            use.rate.model)
    }, {
        stop(paste("Unknown dist = ", distributions))
    })
    return(the.contribution)
}
