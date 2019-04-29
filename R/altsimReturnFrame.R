altsimReturnFrame <-
function (accel.var.mat, nsamsz, centim, theta, distribution,
    relationship, time.units = "Time", kctype = 1, escale = 10000,
    e = rep(1e-04, number.parameters), parameter.fixed = rep(F,
        number.parameters), intercept = T, kprint = 0, maxit = 500,
   debug1= F, randomize = T, my.title = NULL)
{
    number.sim <- 1
    number.cases <- sum(nsamsz + 1)
    nty <- 0
    if (intercept)
        int <- 1
    else int <- 0
    theta.hat <- theta
    distribution.number <- numdist(distribution)
    nsubex <- nrow(accel.var.mat)
    nacvar <- ncol(accel.var.mat)
    param.names <- c(paste("beta", 0:ncol(accel.var.mat), sep = ""),
        "sigma")
    nter <- ncol(accel.var.mat) + int
    number.parameters <- nter + 1
    if (generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        parameter.fixed[number.parameters] <- T
        number.parametersx <- number.parameters - 1
    }
    number.things.returned <- number.parameters + ((number.parameters) *
        (number.parameters + 1))/2 + 2
    y <- matrix(rep(0, number.cases), ncol = 1)
    the.case.weights <- rep(0, number.cases)
    the.censor.codes <- rep(0, number.cases)
    ny <- ncol(y)
    the.xmat <- matrix(1, nrow = number.cases, ncol = nter)
    ndscrat <- 4 * (number.parameters * number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1)
    niscrat <- 2 * (number.parameters + 1)
    if (debug1)
        browser()
    zout <- .Fortran("altsim", the.xmat = as.single(the.xmat),
        y = as.single(y), the.censor.codes = as.single(the.censor.codes),
        the.case.weights = as.single(the.case.weights), number.cases = as.integer(number.cases),
        nter = as.integer(nter), ny = as.integer(ny), nty = as.integer(nty),
        ty = single(number.cases), tc = single(number.cases),
        distribution.number = as.integer(distribution.number),
        gamthr = single(number.cases), parameter.fixed = as.logical(parameter.fixed),
        number.parameters = as.integer(number.parameters), int = as.integer(int),
        escale = as.single(escale), e = as.single(e), maxit = as.integer(maxit),
        kprint = as.integer(kprint), dscrat = double(ndscrat),
        iscrat = integer(niscrat), devian = single(number.cases *
            3), thetah = as.single(theta.hat), first.derivative = single(number.parameters),
        vcv.matrix = single(number.parameters * number.parameters),
        correlation.matrix = single(number.parameters * number.parameters),
        residuals = single(ny * number.cases), fitted.values = single(ny *
            number.cases), theta.real = as.single(theta), the.new.xmat = single(number.cases *
            nter), new.y = single(number.cases * ny), centim = as.single(centim),
        accel.var.mat = as.single(accel.var.mat), nsubex = as.integer(nsubex),
        nacvar = as.integer(nacvar), nsamsz = as.integer(nsamsz),
        kctype = as.integer(kctype), return.matrix = single(number.sim *
            (number.things.returned)), number.things.returned = as.integer(number.things.returned),
        number.sim = as.integer(number.sim), iersim = integer(1))
    if (zout$iersim > 0 || debug1) {
        browser()
        if (zout$iersim > 0)
            stop("Need more space for observations")
    }
    param.names <- c("b0", paste("b", 1:(nter - 1), sep = ""),
        "sigma")
    likelihood <- return.matrix <- t(matrix(zout$return.matrix,
        nrow = number.things.returned))
    theta.hat.star <- return.matrix[, 2:(number.parameters +
        1), drop = F]
    dimnames(theta.hat.star) <- list(NULL, param.names)
    likelihood <- return.matrix[, number.parameters + 2]
    ierstuff <- floor(return.matrix[, 1] + 0.1)
    vcv <- return.matrix[, (number.parameters + 3):(number.parameters +
        ((number.parameters) * (number.parameters + 1))/2 + 2),
        drop = F]
    dimnames(vcv) <- list(NULL, get.vcv.names(param.names))
    the.censor.codes <- zout$the.censor.codes[zout$the.censor.codes >
        0]
    the.case.weights <- zout$the.case.weights[1:length(the.censor.codes)]
    x.columns <- dimnames(accel.var.mat)[[2]]
    the.xmat <- matrix(zout$the.xmat, ncol = nter)[1:length(the.censor.codes),
        -1, drop = F]
    for (j in 1:ncol(the.xmat)) {
        the.xmat[, j] <- f.relationshipinv(the.xmat[, j], subscript.relationship(relationship,
            j))
    }
    y <- zout$y[1:length(the.censor.codes)]
    the.frame <- data.frame(Time = y, the.xmat = the.xmat, Status = the.censor.codes,
        Weights = the.case.weights)
    names(the.frame) <- c(time.units, x.columns, "Status", "Weights")
    if (is.null(my.title))
        my.title <- "Simulated Data"
    the.frame <- frame.to.ld(the.frame, response.column = time.units,
        censor.column = "Status", case.weight.column = "Weights",
        x.columns = x.columns, data.title = my.title)
    attr(the.frame, "ierstuff") <- ierstuff
    attr(the.frame, "likelihood") <- likelihood
    attr(the.frame, "theta.hat.star") <- theta.hat.star
    attr(the.frame, "vcv") <- vcv
    return(the.frame)
}
