get.dist.pnames <-
function (distribution) 
{
    if (!is.character(distribution)) 
        stop("distribution must be character string")
    switch(generic.distribution(distribution), exponential = pnames <- c("log(theta)"), 
        sev = , weibull = , normal = , lognormal = , logistic = , 
        loglogistic = , lev = , frechet = , lev = pnames <- c("mu", 
            "sigma"), gng = , loggng = pnames <- c("mu", "sigma", 
            "shape"), gamma = , loggamma = pnames <- c("scale", 
            "shape"), logbisa = , bisa = pnames <- c("scale", 
            "shape"), logigau = , igau = pnames <- c("scale", 
            "shape"), loggoma = , goma = pnames <- c("scale", 
            "shape1", "shape2"), stop("Distribution name not recognized"))
    return(pnames)
}
