dist.info <-
function (distribution, 
          allow = F) 
{
    collapse.distribution <- paste(distribution, collapse = ",")
    
    distribution <- generic.distribution(collapse.distribution,
                                         allow = allow)
    if (is.null(distribution)) {
        
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- collapse.distribution
        shape <- F
        prob.scale <- "sev"
        idist <- 0
        return(list(idist = idist, 
                    take.logs = take.logs, 
                    num.shape.needed = num.shape.needed, 
                    formal.name = formal.name))
        
    }
    idist <- numdist(distribution)
    switch(distribution, sev = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Smallest Extreme Value"
        shape <- F
        prob.scale <- "sev"
    }, weibull = {
        take.logs <- "if.no.shape"
        num.shape.needed <- 0
        formal.name <- "Weibull"
        prob.scale <- "sev"
    }, uniform = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Uniform"
        prob.scale <- "uniform"
    }, loguniform = {
        take.logs <- "always"
        num.shape.needed <- 0
        formal.name <- "Log-Uniform"
        prob.scale <- "loguniform"
    }, normal = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Normal"
        prob.scale <- "normal"
    }, lognormal = {
        take.logs <- "if.no.shape"
        num.shape.needed <- 0
        formal.name <- "Lognormal"
        prob.scale <- "normal"
    }, logistic = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Logistic"
        prob.scale <- "logistic"
    }, loglogistic = {
        take.logs <- "if.no.shape"
        num.shape.needed <- 0
        formal.name <- "Loglogistic"
        prob.scale <- "logistic"
    }, exponential = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Exponential"
        prob.scale <- "exponential"
    }, gamma = {
        take.logs <- "never"
        num.shape.needed <- 1
        formal.name <- "Gamma"
        prob.scale <- "gamma"
    }, gng = {
        take.logs <- "always"
        num.shape.needed <- 1
        formal.name <- "Generalized Gamma"
        prob.scale <- "gen-gamma"
    }, lev = {
        take.logs <- "never"
        num.shape.needed <- 0
        formal.name <- "Largest Extreme Value"
        prob.scale <- "normal"
    }, frechet = {
        take.logs <- "if.no.shape"
        num.shape.needed <- 0
        formal.name <- "Frechet"
        prob.scale <- "normal"
    }, igau = {
        take.logs <- "never"
        num.shape.needed <- 1
        formal.name <- "Inverse Gaussian"
        prob.scale <- "igau"
    }, bisa = {
        take.logs <- "never"
        num.shape.needed <- 1
        formal.name <- "Birnbaum-Saunders"
        prob.scale <- "bisa"
    }, goma = {
        take.logs <- "never"
        num.shape.needed <- 2
        formal.name <- "Gompertz-Makeham"
        prob.scale <- "goma"
    }, gnf = {
        take.logs <- "always"
        num.shape.needed <- 2
        formal.name <- "Generalized F"
        prob.scale <- "gen-F"
    }, normalgets = {
        take.logs <- "never"
        num.shape.needed <- 1
        formal.name <- "Normal Generalized Threshold Scale"
        prob.scale <- "lognormal"
    }, sevgets = {
        take.logs <- "never"
        num.shape.needed <- 1
        formal.name <- "Smallest Extreme Value Generalized Threshold Scale"
        prob.scale <- "weibull"
    }, egeng = {
        take.logs <- "always"
        num.shape.needed <- 1
        formal.name <- "Extended Generalized Gamma"
        prob.scale <- "weibull"
    }, {
        if (allow) return(list())
        stop("Distribution not recognized")
    })
    
    return(list(idist = idist, 
                take.logs = take.logs, 
                num.shape.needed = num.shape.needed, 
                formal.name = formal.name))
}
