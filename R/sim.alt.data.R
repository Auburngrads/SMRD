sim.alt.data <-
function (object, 
          nsamsz, 
          centim, 
          theta, 
          distribution,
          xlab, 
          data.title, 
          kctype, 
          time.units = "Time",
          intercept = T,
          kprint = 0,
          debug1 = F, 
          randomize = T,...)
{
    number.cases <- sum(nsamsz)
    plan <- list(object = object, 
                 nsamsz = nsamsz,
                 centim = centim)
    
    `if`(intercept, int <- 1, int <- 0)
    nter <- ncol(object) + int
    number.parameters <- nter + 1
    distribution.number <- numdist(distribution)
    y <- matrix(rep(0, number.cases), ncol = 1)
    the.case.weights <- rep(0, number.cases)
    the.censor.codes <- rep(0, number.cases)
    ny <- ncol(y)
    nty <- 0
    the.xmat <- matrix(444, 
                       nrow = number.cases, 
                       ncol = ncol(object) + int)
    
    if (missing(data.title))
        data.title <- paste("Simulated ALT n=", paste(nsamsz,
            collapse = ","), , distribution, "Distribution with \n centime=",
            paste(centim, collapse = ","), "parameters=", paste(format(theta),
                collapse = ","))
    
    if (missing(xlab)) xlab <- paste("X", 1:ncol(object), sep = "")
    if (debug1) browser()
    
    zout <- wqm_simalt(theta = as.single(theta), 
                       nparm = as.integer(number.parameters),
                       intcpt = as.integer(int), 
                       nsamsiz = as.integer(nsamsz),
                       kctype = as.integer(kctype), 
                       centim = as.single(centim),
                       acvar = as.matrix(object), 
                       nsubex = as.integer(nrow(object)),
                       nacvar = as.integer(ncol(object)), 
                       kdist = as.integer(distribution.number),
                       x = the.xmat, 
                       y = y, 
                       cen = integer(number.cases),
                       wt = integer(number.cases), 
                       nrow = as.integer(number.cases),
                       nter = as.integer(nter), 
                       ny = as.integer(ny), 
                       nty = as.integer(nty),
                       ty = matrix(0,nrow = number.cases, ncol = 1), 
                       tc = integer(number.cases),
                       nrownw = integer(1), 
                       iersim = integer(1), 
                       kprint = as.integer(kprint))
    
    if (zout$iersim > 0) stop(paste("iersim = ", zout$iersim))
    nrownw <- zout$nrownw
    y <- matrix(zout$y, ncol = 1)[1:nrownw, ]
    the.censor.codes <- zout$cen
    length(the.censor.codes) <- nrownw
    the.case.weights <- zout$wt
    length(the.case.weights) <- nrownw
    the.xmat <- matrix(zout$x, ncol = nter)[1:nrownw,]
    if (intercept) the.xmat <- the.xmat[, -1]
    
    return(make.frame.ld(y = y, 
                         the.censor.codes = the.censor.codes,
                         the.case.weights = the.case.weights, 
                         time.units = time.units,
                         the.xmat = the.xmat, 
                         xlabel = xlab, 
                         data.title = data.title))
}
