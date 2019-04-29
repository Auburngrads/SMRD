numdist <-
function (distribution, allow = F)
{
    the.distribution <- generic.distribution(distribution, allow = allow)
    
    if (is.null(the.distribution) || distribution == "") {
      
        the.message <- paste("Distribution not recognized in numdist:",
                             distribution)
        `if`(allow,
             idist <- 0,
             stop(the.message))
        
    } else {
    
        switch(generic.distribution(distribution, allow = allow),
            sev = idist <- 1, weibull = idist <- 2, normal = idist <- 3,
            lognormal = idist <- 4, logistic = idist <- 5, loglogistic = idist <- 6,
            lev = idist <- 7, frechet = idist <- 8, gng = idist <- 10,
            gamma = idist <- 12, logexponential = idist <- 13,
            exponential = idist <- 14, igau = idist <- 16, bisa = idist <- 18,
            goma = idist <- 20, gnf = idist <- 22, uniform = idist <- 23,
            loguniform = idist <- 24, egeng = idist <- 26, sevgets = idist <- 27,
            normalgets = idist <- 29, levgets = idist <- 30,
            beta = idist <- 31, logbeta = idist <- 32, triangle = idist <- 33,
            logtriangle = idist <- 34, )
    }
    
    return(idist)
}
