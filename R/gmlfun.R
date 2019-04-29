gmlfun <-
function (gmlest.out, 
          kfunc = 2, 
          fargv = NULL, 
          kpopu = 0, 
          kpoint = 0,
          kprint = 0, 
          conf.level = GetSMRDDefault("SMRD.ConfLevel") / 100, 
          debug1 = F)
{
    data.ld <- gmlest.out$data.ld
    distribution <- gmlest.out$distribution
    model <- gmlest.out$model
    explan.vars <- gmlest.out$explan.vars
    kfuncp <- 10 * kpopu + kfunc
    y <-Response(data.ld)
    ncoly <- ncol(y)
    number.cases <- nrow(y)
    the.case.weights <- SMRD:::case.weights(data.ld)
    ny <- ncol(y)
    the.truncation.codes <- SMRD:::truncation.codes(data.ld)
    
    if (is.null(the.truncation.codes)) {
        ty <- 1
        ncolty <- 0
        the.truncation.codes <- 1
        
    } else {
        
        ty <- truncation.response(data.ld)
        ncolty <- ncol(ty)
        
    }
    
    distribution.number <- numdist(gmlest.out$distribution)
    the.censor.codes <- censor.codes(data.ld)
    get.rmodel.info.out <- get.rmodel.info(distribution, 
                                           model,
                                           explan.vars)
    
    if (get.rmodel.info.out$nrelat == 0) {
        
        regression <- F
        ncol.orig.x <- 0
        int <- 1
        the.xmat <- cbind(rep(1, number.cases))
        
    } else {
        
        the.xmat <- xmat(data.ld)
        ncol.orig.x <- ncol(the.xmat)
        regression <- T
        
        if (regression) {
            
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
    kparv <- get.rmodel.info.out$kparv
    nrvar <- get.rmodel.info.out$nrvar
    mrelat <- get.rmodel.info.out$mrelat
    nrelat <- max(get.rmodel.info.out$nrelat, 1)
    mnrvar <- max(get.rmodel.info.out$nrvar)
    
    switch(kfunc, {
        
        kodef <- 3
        if (is.null(fargv)) fargv <- seq(min(y), max(y), length = 40)
        
    }, {
        
        kodef <- 2
        if (is.null(fargv)) fargv <- c(0.001, 0.005, 0.01, 0.05,
            0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95,
            0.99)
        
    }, {
        
        kodef <- 2
        if (is.null(fargv)) fargv <- seq(min(y), max(y), length = 40)
        
    })
    
    if (debug1) browser()
    
    zout <- .Fortran("genfun", 
                     as.integer(model), 
                     as.integer(distribution.number),
                     ilabp = integer(8 * nparm), 
                     ilabd = integer(8 * nparm),
                     theta.hat = as.double(gmlest.out$theta.hat), 
                     thetas.hat = as.double(gmlest.out$thetas.hat),
                     kodet = integer(nparm), 
                     as.integer(gmlest.out$parameter.fixed),
                     as.integer(nparm), 
                     as.integer(npard), 
                     as.single(y), 
                     as.integer(ncoly),
                     as.integer(number.cases), 
                     as.single(the.xmat), 
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
                     as.double(conf.level), 
                     as.integer(kodef), 
                     as.double(fargv),
                     as.integer(length(fargv)), 
                     as.integer(kfuncp), 
                     as.integer(kpopu),
                     as.integer(kpoint), 
                     as.double(gmlest.out$vcvs.vector),
                     fest = double(length(fargv)), 
                     stderr = double(length(fargv)),
                     xlow = double(length(fargv)), 
                     xup = double(length(fargv)))
    
    fest <- zout$fest
    stderr <- zout$stderr
    xlow <- zout$xlow
    xup <- zout$xup
    
    return.list <- list(fargv = fargv, 
                        fest = fest, 
                        stderr = stderr,
                        xlow = xlow, 
                        xup = xup)
    
    return(return.list)
}
