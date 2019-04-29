mlesim.stagger <-
function (numsim, distribution, theta, censor.scheme, sample.size,
    predict.time.delta = 1, explan.vars = NULL, escale = 10000,
    e = rep(1e-04, number.parameters), parameter.fixed = rep(F,
        number.parameters), intercept = T, kprint = 0, maxit = 500,
   debug1= F, kmeth = NULL, randomize = T)
{
    censor.times <- censor.scheme$censor.times
    tr.censor.times <- ifelse(is.logdist(distribution), logb(censor.times),
        censor.times)
    pfail.vec <- wqmf.phibf((tr.censor.times - theta[1])/theta[2],
        distribution)
    efail <- sum(censor.scheme$number.units * pfail.vec)
    sdfail <- sqrt(efail)
    max.number.cases <- max(efail + 5 * sdfail, 500)
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
    ndscrat <- number.parameters * max.number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1
    niscrat <- 2 * (number.parameters + 1)
    ngroup <- length(censor.scheme$number.units)
    krfail <- rep(0, ngroup)
    number.things.returned <- 7
    if (debug1)
        browser()
    zout <- .Fortran("mlsim6", xmat = single(nter * max.number.cases),
        y = single(ny * max.number.cases), cen = single(max.number.cases),
        wt = single(max.number.cases), nrow = as.integer(max.number.cases),
        nter = as.integer(nter), ny = as.integer(ny), nty = as.integer(nty),
        ty = single(max.number.cases), truncation.codes = single(max.number.cases),
        kdist = as.integer(distribution.number), gamthr = single(max.number.cases),
        lfix = as.logical(parameter.fixed), krfail = as.integer(krfail),
        nparm = as.integer(number.parameters), int = as.integer(int),
        escale = as.single(escale), e = as.single(e), maxit = as.integer(maxit),
        kprint = as.integer(kprint), dscrat = double(ndscrat),
        iscrat = integer(niscrat), devian = single(max.number.cases),
        thetah = single(number.parameters), first.derivative = single(number.parameters),
        vcv.matrix = single(number.parameters * number.parameters),
        correlation.matrix = single(number.parameters * number.parameters),
        residuals = single(ny * max.number.cases), fitted.values = single(ny *
            max.number.cases), theta = as.single(theta), return.matrix = single(numsim *
            number.things.returned), numsim = as.integer(numsim),
        prdelt = as.single(predict.time.delta), ngroup = as.integer(ngroup),
        centim = as.single(censor.scheme$censor.times), nsamsz = as.integer(censor.scheme$number.units),
        nmrvec = integer(ngroup), nsimg = integer(1), numret = as.integer(7),
        nnomle = integer(1), iersim = integer(1))
    if (zout$iersim > 0 || debug1) {
        browser()
        if (zout$iersim > 0)
            stop("Need more space for observations")
    }
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    theta.hat.star <- return.matrix[, 2:(number.parameters +
        1)]
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    return.list <- list(theta.hat.star = theta.hat.star, loglikelihood = return.matrix[,
        4], ierstuff = ierstuff, numsim = numsim, kmeth = kmeth)
    return(return.list)
}
