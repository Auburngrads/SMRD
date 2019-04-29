gmlest <-
function (data.ld, 
          distribution, 
          theta.start = NULL, 
          explan.vars = NULL,
          mu.relat = NULL, 
          sigma.relat = NULL, 
          prob.relat = NULL, 
          gamthr = 0,
          escale = 10000, 
          e = rep(1e-04, nparm), 
          parameter.fixed = rep(F, nparm), 
          intercept = T, 
          model = 0, 
          kprint = 0, 
          conlev = 0.95,
          maxit = 500, 
          debug1 = F)
{
    y <-Response(data.ld)
    ncoly <- ncol(y)
    number.cases <- nrow(y)
    the.case.weights <- case.weights(data.ld)
    ny <- ncol(y)
    the.truncation.codes <- truncation.codes(data.ld)
    if (is.null(the.truncation.codes)) {
        ty <- 1
        ncolty <- 0
        the.truncation.codes <- 1
  
        } else {
          
        ty <- truncation.response(data.ld)
        ncolty <- ncol(ty)
    
        }
    distribution.number <- numdist(distribution)
    
    if (distribution.number == 14) distribution.number <- 8
    
    cat("dist num =", distribution, distribution.number, "\n")
    the.censor.codes <- censor.codes(data.ld)
    
    if (length(gamthr) == 1)
        gamthr <- rep(gamthr, number.cases)
    
    if (length(gamthr) != number.cases)
        stop("specified offset is the wrong length")
    
    get.rmodel.info.out <- get.rmodel.info(distribution, 
                                           model, 
                                           explan.vars)
    explan.vars <- get.rmodel.info.out$explan.vars
    
    if (get.rmodel.info.out$nrelat == 0) {
      
        regression <- F
        ncol.orig.x <- 0
        int <- 1
        the.xmat <- cbind(rep(1, number.cases))
  
        } else {
          
        the.xmat <- xmat(data.ld)
        ncol.orig.x <- ncol(the.xmat)
        if (is.null(the.xmat))
            stop("Explanatory variables requested, but there is no X matrix")
        regression <- T
        if (nrow(the.xmat) != number.cases)
            stop(paste("Number of rows in x matrix ", nrow(the.xmat),
                " is wrong"))
        uniq.explan.vars <- unique(get.rmodel.info.out$mrelat)
        
        if (any(uniq.explan.vars<=0))
            stop("Negative or 0 explanatory variables column specified")
        
        if (max(uniq.explan.vars) > ncol(the.xmat))
            stop("Specified explanatory variable column greater than number of columns in X matrix")
        
        if (intercept) {
            
              int <- 1
              the.xmat <- cbind(rep(1, number.cases), the.xmat)
      
            } else {
            
              int <- 0
        
            }
    
        }
    zsize <- .Fortran("gensiz", 
                      as.integer(model), 
                      as.integer(distribution.number),
                      as.integer(get.rmodel.info.out$kparv), 
                      as.integer(get.rmodel.info.out$nrvar),
                      as.integer(get.rmodel.info.out$mrelat), 
                      as.integer(get.rmodel.info.out$nrelat),
                      as.integer(max(get.rmodel.info.out$nrvar)), 
                      as.integer(ncol.orig.x),
                      as.integer(kprint), 
                      nparm = integer(1), 
                      npard = integer(1),
                      ier = integer(1),
                      nxd = integer(5),
                      intd = integer(5),
                      ipxcd = integer(5),
                      irelad = integer(5),
                      index = integer(1),
                      nterd = integer(1),
                      itry = integer(1),
                      ipb = integer(1),
                      ips = integer(1),
                      ipe = integer(1),
                      iis = integer(200))
    
    nparm <- zsize$nparm
    npard <- zsize$npard
    
    if (is.null(theta.start)) {
      
        lstart <- 0
        theta.start <- double(nparm)
  
        } else {
        
        lstart <- 1
    
        }
    
    if (any(parameter.fixed) == T) {
      
        `if`(is.null(theta.start),
             stop("Fixed parameters requested, but theta.start empty"),
             theta <- theta.start)

        } else {
          
        theta <- double(nparm)
    
        }
    kparv <- get.rmodel.info.out$kparv
    nrvar <- get.rmodel.info.out$nrvar
    mrelat <- get.rmodel.info.out$mrelat
    nrelat <- max(get.rmodel.info.out$nrelat, 1)
    mnrvar <- max(get.rmodel.info.out$nrvar)
    
    if (debug1) browser()
    
    zout <- .Fortran("genmax", 
                     as.integer(model), 
                     as.integer(distribution.number),
                     as.double(theta), 
                     double(nparm),
                     kodet = integer(nparm), 
                     as.integer(parameter.fixed),
                     as.integer(nparm), 
                     as.integer(npard), 
                     as.single(as.matrix(y)), 
                     as.integer(ncoly),
                     as.integer(number.cases), 
                     as.single(as.matrix(the.xmat)),
                     as.integer(ncol.orig.x), 
                     as.single(the.censor.codes),
                     as.single(the.case.weights), 
                     as.single(ty), 
                     as.integer(ncolty),
                     as.single(the.truncation.codes), 
                     as.integer(kprint),
                     as.integer(kparv), 
                     as.integer(nrvar), 
                     as.integer(mrelat),
                     as.integer(nrelat), 
                     as.integer(mnrvar), 
                     xlogl = double(1),
                     yhat = single(length(y)), 
                     resid = single(length(y)),
                     vcvs = double(nparm * nparm), 
                     vcv = double(nparm * nparm), 
                     r = double(nparm * nparm),
                     as.double(theta.start), 
                     as.integer(lstart), 
                     as.double(conlev),
                     ilabp = integer(8 * nparm), 
                     ilabd = integer(8 * nparm),
                     ier = integer(1))
    if (zout$ier > 0)
        warning(paste("Genmax error messages estimation/vcv",
            zout$ier))
    log.likelihood <- zout$log.likelihood
    thetas.hat <- zout$thetas.hat
    theta.hat <- zout$theta.hat
    kodet <- zout$kodet
    names(theta.hat) <- get.rmodel.info.out$model.pnames
    first.derivative <- zout$first.derivative
    names(parameter.fixed) <- get.rmodel.info.out$model.pnames
    correlation.matrix <- matrix(zout$correlation.vector, ncol = nparm)
    matnames <- list(get.rmodel.info.out$model.pnames, get.rmodel.info.out$model.pnames)
    vcv.matrix <- matrix(zout$vcv.vector, ncol = nparm)
    vcvs.vector <- zout$vcvs.vector
    dimnames(correlation.matrix) <- matnames
    dimnames(vcv.matrix) <- matnames
    time.units<-attr(data.ld, "time.units")
    if (regression) {
        fitted.values <- zout$fitted.values
        residuals <- matrix(zout$residuals, ncol = ncoly)
        the.list <- list(data.ld = data.ld, model = model, distribution = distribution,
            parameter.fixed = parameter.fixed, explan.vars = explan.vars,
            log.likelihood = log.likelihood, theta.hat = theta.hat,
            thetas.hat = thetas.hat, correlation.matrix = correlation.matrix,
            vcv.matrix = vcv.matrix, vcvs.vector = vcvs.vector,
            kodet = kodet, residuals = residuals, fitted.values = fitted.values,
            get.rmodel.info.out = get.rmodel.info.out, time.units = time.units)
  } else {
        the.list <- list(data.ld = data.ld, model = model, distribution = distribution,
            parameter.fixed = parameter.fixed, explan.vars = explan.vars,
            log.likelihood = log.likelihood, theta.hat = theta.hat,
            thetas.hat = thetas.hat, correlation.matrix = correlation.matrix,
            vcv.matrix = vcv.matrix, vcvs.vector = vcvs.vector,
            kodet = kodet, time.units = time.units)
    }
    oldClass(the.list) <- "mlest"
    return(the.list)
}
