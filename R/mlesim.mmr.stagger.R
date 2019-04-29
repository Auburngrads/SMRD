mlesim.mmr.stagger <-
function (numsim, 
          theta, 
          censor.scheme, 
          distribution = "Weibull",
          predict.time.delta = 1, 
          explan.vars = NULL, 
          escale = 10000,
          e = rep(1e-04, number.parameters), 
          parameter.fixed = rep(F, number.parameters), 
          intercept = T, 
          kprint = 0, 
          maxit = 500,
          debug1= F, 
          randomize = T)
{
    censor.times <- censor.scheme$censor.times
    tr.censor.times <- ifelse(is.logdist(distribution), 
                              logb(censor.times),
                              censor.times)
    
    pfail.vec <- wqmf.phibf((tr.censor.times - theta[1])/theta[2],distribution)
    efail <- sum(censor.scheme$number.units * pfail.vec)
    sdfail <- sqrt(efail)
    number.intervals <- length(censor.scheme[[1]])
    max.number.cases <- max(efail + 7 * sdfail + 2 * number.intervals,
                            500 + 2 * number.intervals)
    nty <- 0
    ny <- 1
    nter <- 1
    int <- 1
    distribution.number <- numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    if (generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        theta.start[number.parameters] <- 1
        parameter.fixed[number.parameters] <- T
    }
    ndscrat <- 4 * max.number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1
    niscrat <- 2 * (number.parameters + 1) + 2 * max.number.cases
    ngroup <- length(censor.scheme$number.units)
    krfail <- rep(0, ngroup)
    number.things.returned <- 5
    if (as.numeric(debug1) >= 1) { }
    if (as.numeric(debug1) >= 3) browser()

        zout <- .Fortran("mlsim7", 
                         xmat = single(nter * max.number.cases),
                         y = single(ny * max.number.cases), 
                         cen = single(max.number.cases),
                         wt = single(max.number.cases), 
                         nrow = as.integer(max.number.cases),
                         nter = as.integer(nter), 
                         ny = as.integer(ny), 
                         nty = as.integer(nty),
                         ty = single(max.number.cases), 
                         truncation.codes = single(max.number.cases),
                         kdist = as.integer(distribution.number), 
                         gamthr = single(max.number.cases),
                         lfix = as.logical(parameter.fixed), 
                         krfail = as.integer(krfail),
                         nparm = as.integer(number.parameters), 
                         int = as.integer(int),
                         escale = as.single(escale), 
                         e = as.single(e), 
                         maxit = as.integer(maxit),
                         kprint = as.integer(kprint), 
                         dscrat = double(100 * ndscrat), 
                         iscrat = integer(100 * niscrat), 
                         devian = single(max.number.cases),
                         thetah = single(number.parameters), 
                         first.derivative = single(number.parameters),
                         vcv.matrix = single(number.parameters * number.parameters),
                         correlation.matrix = single(number.parameters * number.parameters),
                         residuals = single(ny * max.number.cases), 
                         fitted.values = single(ny * max.number.cases), 
                         theta = as.single(theta),
                         return.matrix = single(numsim * number.things.returned),
                         numsim = as.integer(numsim), 
                         prdelt = as.single(predict.time.delta),
                         ngroup = as.integer(ngroup), 
                         centim = as.single(censor.scheme$censor.times),
                         nsamsz = as.integer(censor.scheme$number.units),
                         nmrvec = integer(ngroup), 
                         nsimg = integer(1), 
                         numret = as.integer(number.things.returned),
                         nnomle = integer(1), 
                         iersim = integer(1))

    if (as.numeric(debug1) > 2)
        browser()
    if (zout$iersim > 0)
        stop("Need more space for observations")
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    theta.hat.star <- return.matrix[ierstuff == 0, 2:5]
    attr(theta.hat.star, "distribution") <- distribution
    attr(theta.hat.star, "ierstuff") <- ierstuff
    attr(theta.hat.star, "theta") <- theta
    attr(theta.hat.star, "numsim") <- numsim
    attr(theta.hat.star, "censor.scheme") <- censor.scheme
    return(theta.hat.star)
}
