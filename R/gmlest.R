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
    zsize <- GENSIZ(as.integer(model),
                    as.integer(distribution.number),
                    as.integer(get.rmodel.info.out$kparv),
                    as.integer(get.rmodel.info.out$nrvar),
                    get.rmodel.info.out$mrelat,
                    as.integer(get.rmodel.info.out$nrelat),
                    as.integer(max(get.rmodel.info.out$nrvar)),
                    as.integer(ncol.orig.x),
                    as.integer(kprint),
                    nparm = integer(1),
                    npard = integer(1),
                    ier = integer(1),
                    nxd = as.integer(rep(0,5)),
                    intd = as.integer(rep(1000,5)),
                    ipxcd = vector(mode = "list", length = 5),
                    irelad = as.integer(rep(1,5)),
                    ilabp = as.integer(rep(0,80)),
                    ilabd = as.integer(rep(0,40)),
                    nregr = as.integer(0),
                    kgtall = as.integer(1),
                    llog = as.integer(0),
                    kmodp = as.integer(0),
                    npardm = as.integer(5),
                    nnum = as.integer(0),
                    kparm = as.integer(0),
                    iup = as.integer(0),
                    nterd = as.integer(0),
                    maxpd = as.integer(20))
    
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
    
    zout <-  GENMAX(as.integer(model), 
                    as.integer(distribution.number),
                    as.double(rep(0,nparm)), 
                    double(nparm),
                    kodet = integer(nparm), 
                    as.integer(parameter.fixed),
                    as.integer(nparm), 
                    as.integer(npard), 
                    as.matrix(y), 
                    as.integer(ncoly),
                    as.integer(number.cases), 
                    as.matrix(the.xmat),
                    as.integer(ncol.orig.x), 
                    as.integer(the.censor.codes),
                    as.integer(the.case.weights), 
                    as.matrix(ty), 
                    as.integer(ncolty),
                    as.integer(the.truncation.codes), 
                    as.integer(kprint),
                    as.integer(kparv), 
                    as.integer(nrvar), 
                    as.matrix(mrelat),
                    as.integer(nrelat), 
                    as.integer(mnrvar), 
                    xlogl = double(1),
                    yhat = matrix(0,dim(y)), 
                    resid = matrix(0,dim(y)),
                    vcvs = matrix(0,nparm, nparm), 
                    vcv = matrix(0,nparm, nparm), 
                    r = matrix(0,nparm, nparm),
                    as.double(theta.start), 
                    as.integer(lstart), 
                    as.double(conlev),
                    ilabp = integer(8 * nparm), 
                    ilabd = integer(8 * nparm),
                    ier = integer(1),
                    nxd = as.integer(rep(0,5)),
                    intd = as.integer(rep(1000,5)),
                    ipxcd = list(0,0,0,0,0),
                    irelad = as.integer(rep(1,5)),
                    fstder = double(12),
                    nregr = as.integer(0), 
                    kcentr = as.integer(1), 
                    kpoint = as.integer(0), 
                    ifit   = as.integer(2),
                    kgtall = as.integer(1), 
                    llog   = as.integer(0), 
                    kmodp  = as.integer(0),
                    maxit  = as.integer(50),
                    pest = as.double(1.0), 
                    epsx = as.double(1.0e-10),
                    npardm = as.integer(5),
                    nnum = as.integer(0),
                    kparm = as.integer(0),
                    iup = as.integer(0),
                    nterd = as.integer(0),
                    maxpd = as.integer(20),
                    pfail = as.double(0),
                    kmccde = as.integer(0),
                    nstart = as.integer(0),
                    maxmsd = as.integer(1),
                    tol    = as.double(1.0e-2),
                    lsd = as.integer(1),
                    pchmax = as.double(0))
    
    if(zout$ier > 0) warning(paste("Genmax error messages estimation/vcv",zout$ier))
    
    log.likelihood <- zout$doubs$xlogl
    thetas.hat <- zout$numvec$thetas
    theta.hat <- zout$numvec$theta.hat
    kodet <- zout$intvec$kodet
    names(theta.hat) <- get.rmodel.info.out$model.pnames
    first.derivative <- zout$numvec$fsder
    names(parameter.fixed) <- get.rmodel.info.out$model.pnames
    correlation.matrix <- matrix(zout$nummat$r, ncol = nparm)
    matnames <- list(get.rmodel.info.out$model.pnames, get.rmodel.info.out$model.pnames)
    vcv.matrix <- matrix(zout$nummat$vcv, ncol = nparm)
    vcvs.vector <- zout$nummat$vcvs
    dimnames(correlation.matrix) <- matnames
    dimnames(vcv.matrix) <- matnames
    time.units<-attr(data.ld, "time.units")
    if (regression) {
        fitted.values <- zout$nummat$yhat
        residuals <- matrix(zout$nummat$residuals, ncol = ncoly)
        the.list <- list(data.ld = data.ld, 
                         model = model, 
                         distribution = distribution,
                         parameter.fixed = parameter.fixed,
                         explan.vars = explan.vars,   
                         log.likelihood = log.likelihood, 
                         theta.hat = theta.hat,
                         thetas.hat = thetas.hat, 
                         correlation.matrix = correlation.matrix, 
                         vcv.matrix = vcv.matrix, 
                         vcvs.vector = vcvs.vector,
                         kodet = kodet, 
                         residuals = residuals, 
                         fitted.values = fitted.values,
                         get.rmodel.info.out = get.rmodel.info.out, 
                         time.units = time.units)
      } else {
        
        the.list <- list(data.ld = data.ld, 
                         model = model, 
                         distribution = distribution,
                         parameter.fixed = parameter.fixed, 
                         explan.vars = explan.vars,
                         log.likelihood = log.likelihood, 
                         theta.hat = theta.hat,
                         thetas.hat = thetas.hat, 
                         correlation.matrix = correlation.matrix,
                         vcv.matrix = vcv.matrix, 
                         vcvs.vector = vcvs.vector,
                         kodet = kodet, 
                         time.units = time.units)
        
      }
    
    oldClass(the.list) <- "mlest"
    return(the.list)
    
}
