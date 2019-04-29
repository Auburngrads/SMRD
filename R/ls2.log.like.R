ls2.log.like <-
function (theta)
{
    model <- get(envir = .frame0,  "model")
    distribution <- (model$distribution)
    if (is.null(dim(theta))) {
        sigt <- (theta[2])
        mut <- theta[1]
        number <- 1
        if (sigt <= 1e-05)
            return(1e+10)
  } else {
        sigt <- (theta[, 2])
        mut <- theta[, 1]
        number <- length(mut)
    }
    data.ld <- get(envir = .frame0,  "data.ld")
   debug1<- get(envir = .frame0,  "debug1")
    if (debug1> 2)
        kprint <- 1
    else kprint <- 0
    return(-like.eval(data.ld, distribution, cbind(mut, sigt),
        explan.vars = NULL))
}
