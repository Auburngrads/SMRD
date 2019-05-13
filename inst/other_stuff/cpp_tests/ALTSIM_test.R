library(SMRD)
accel.var.mat <- cbind(V1=c(1,1,2,2),V2=c(1,2,1,2))
nsamsz = c(4,4,4,4)
centim = c(40000,40000,40000,40000)
theta = c(1,-24,5,1)
distribution = 'normal'
number.sim = 10
kctype = 1
escale = 10000
intercept = T
kprint = 0
maxit = 500
debug1= F
randomize = T
number.cases <- sum(nsamsz + 1)
    plan <- list(accel.var.mat = accel.var.mat,
                 nsamsz = nsamsz,
                 centim = centim)
    nty <- 0
    `if`(intercept, int <- 1, int <- 0)
    theta.hat <- theta
    distribution.number <- SMRD:::numdist(distribution)
    if (is.null(accel.var.mat)) {
        accel.var.mat <- 0
        if (int != 1) stop("must have int=1 if no x matrix")
        param.names <- c("mu", "sigma")
        nter <- 1
        nsubex <- 1
        nacvar <- 0
  } else {
        nsubex <- nrow(accel.var.mat)
        nacvar <- ncol(accel.var.mat)
        param.names <- c(paste("beta", 0:ncol(accel.var.mat), sep = ""), "sigma")
        nter <- ncol(accel.var.mat) + int
    }
    number.parameters <- nter + 1
    if (SMRD:::generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        parameter.fixed[number.parameters] <- T
        number.parametersx <- number.parameters - 1
    }
    number.things.returned <- number.parameters + ((number.parameters) *
        (number.parameters + 1))/2 + 2
    y <- matrix(rep(0, number.cases), ncol = 1)
    case.weights <- rep(0, number.cases)
    censor.codes <- rep(0, number.cases)
    ny <- ncol(y)
    xmat <- matrix(1, nrow = number.cases, ncol = nter)
    ndscrat <- 4 * (number.parameters * number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1)
    niscrat <- 2 * (number.parameters + 1)
    if (debug1) browser()
    e = rep(1e-04, number.parameters)
    parameter.fixed = rep(F, number.parameters)

    zout <- .Fortran("altsim", 
                     xmat = as.single(xmat), 
                     y = as.single(y),
                     censor.codes = as.single(censor.codes),
                     case.weights = as.single(case.weights),
                     number.cases = as.integer(number.cases), 
                     nter = as.integer(nter),
                     ny = as.integer(ny), 
                     nty = as.integer(nty), 
                     ty = single(number.cases),
                     tc = single(number.cases), 
                     distribution.number = as.integer(distribution.number),
                     gamthr = single(number.cases), 
                     parameter.fixed = as.logical(parameter.fixed),
                     number.parameters = as.integer(number.parameters), 
                     intcpt = as.integer(int),
                     escale = as.single(escale), 
                     e = as.single(e), 
                     maxit = as.integer(maxit),
                     kprint = as.integer(kprint), 
                     dscrat = double(ndscrat),
                     iscrat = integer(niscrat), 
                     devian = single(number.cases * 3), 
                     thetah = as.single(theta.hat), 
                     first.derivative = single(number.parameters),
                     vcv.matrix = single(number.parameters * number.parameters),
                     correlation.matrix = single(number.parameters * number.parameters),
                     residuals = single(ny * number.cases), 
                     fitted.values = single(ny * number.cases), 
                     theta.real = as.single(theta), 
                     new.xmat = single(number.cases * nter),
                     new.y = single(number.cases * ny), 
                     centim = as.single(centim),
                     accel.var.mat = as.single(accel.var.mat), 
                     nsubex = as.integer(nsubex),
                     nacvar = as.integer(nacvar), 
                     nsamsz = as.integer(nsamsz),
                     kctype = as.integer(kctype), 
                     return.matrix = single(number.sim *  (number.things.returned)), 
                     number.things.returned = as.integer(number.things.returned),
                     number.sim = as.integer(number.sim), 
                     iersim = integer(1))

    new <- SMRD2::altsim(x = xmat, 
                             y = y,
                             cen = as.integer(censor.codes),
                             wt = as.integer(case.weights),
                             nrow = as.integer(number.cases), 
                             nter = as.integer(nter),
                             ny = as.integer(ny), 
                             nty = as.integer(nty), 
                             ty = matrix(0, nrow = number.cases, ncol = 1),
                             tc = integer(number.cases), 
                             kdist = as.integer(distribution.number),
                             gamthr = double(number.cases), 
                             lfix = as.logical(parameter.fixed),
                             nparm = as.integer(number.parameters), 
                             intcpt = as.integer(int),
                             escale = as.double(escale), 
                             e = as.double(e), 
                             maxit = as.integer(maxit),
                             kprint = as.integer(kprint), 
                             dscrat = double(ndscrat),
                             iscrat = integer(niscrat), 
                             dev = matrix(0,nrow = number.cases, ncol = 3), 
                             thetah = as.double(theta.hat), 
                             fsder = double(number.parameters),
                             vcv = matrix(0,nrow = number.parameters, ncol = number.parameters),
                             r = matrix(0,nrow = number.parameters, ncol = number.parameters),
                             res = matrix(0, ncol = ny, nrow = number.cases), 
                             fv = matrix(0,ncol = ny, nrow = number.cases), 
                             theta = as.double(theta), 
                             xnew = matrix(0, nrow = number.cases, ncol = nter),
                             ynew = matrix(0, nrow = number.cases, ncol = ny), 
                             centim = as.double(centim),
                             acvar = accel.var.mat, 
                             nsubex = as.integer(nsubex),
                             nacvar = as.integer(nacvar), 
                             nsamsz = as.integer(nsamsz),
                             krfail = integer(nsubex),
                             kctype = as.integer(kctype), 
                             retmat = matrix(0, ncol = number.sim, nrow = number.things.returned), 
                             numret = as.integer(number.things.returned),
                             numsim = as.integer(number.sim), 
                             iersim = integer(1))
