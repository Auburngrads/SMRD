library(smrdfortran)
library(SMRD)
test = 1

if(test == 1){
    
    root = here::here()
    source(file.path(root, "inst","test_objs","superalloy_gmlest.R"))

}

kfunc = 2
fargv = NULL
kpopu = 0
kpoint = 0
if(!exists("kprint")) kprint = 0 
conf.level = SMRD:::GetSMRDDefault("SMRD.ConfLevel") / 100
debug1 = F

    data.ld <- gmlest.out$data.ld
    distribution <- gmlest.out$distribution
    model <- gmlest.out$model
    explan.vars <- gmlest.out$explan.vars
    kfuncp <- 10 * kpopu + kfunc
    y <- SMRD:::Response(data.ld)
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
        
        ty <- SMRD:::truncation.response(data.ld)
        ncolty <- ncol(ty)
        
    }
    if(is.null(model)) model = 0
    distribution.number <- SMRD:::numdist(gmlest.out$distribution)
    the.censor.codes    <- SMRD:::censor.codes(data.ld)
    get.rmodel.info.out <- SMRD:::get.rmodel.info(distribution, 
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
    if(is.null(gmlest.out$thetas.hat)) gmlest.out$thetas.hat = rep(0,length(gmlest.out$theta.hat))
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
                     as.double(conf.level),
                     as.integer(kodef),
                     as.double(fargv),
                     as.integer(length(fargv)),
                     as.integer(kfuncp),
                     as.integer(kpopu),
                     as.integer(kpoint),
                     as.double(gmlest.out$vcv.matrix),
                     fest = double(length(fargv)),
                     stderr = double(length(fargv)),
                     xlow = double(length(fargv)),
                     xup = double(length(fargv)))

    new <- SMRD:::GENFUN(as.integer(model), 
                   as.integer(distribution.number),
                   ilabp = integer(8 * nparm), 
                   ilabd = integer(8 * nparm),
                   theta = as.double(gmlest.out$theta.hat), 
                   thetas = as.double(gmlest.out$thetas.hat),
                   kodet = integer(nparm), 
                   as.integer(gmlest.out$parameter.fixed),
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
                   as.double(conf.level), 
                   as.integer(kodef), 
                   as.double(fargv),
                   as.integer(length(fargv)), 
                   as.integer(kfuncp), 
                   as.integer(kpopu),
                   as.integer(kpoint), 
                   as.matrix(gmlest.out$vcv.matrix),
                   fest = double(length(fargv)), 
                   std_err = double(length(fargv)),
                   xlow = double(length(fargv)), 
                   xup = double(length(fargv)),
                   nxd = as.integer(rep(0,5)),
                   intd = as.integer(rep(1000,5)),
                   ipxcd = list(0,0,0,0,0),
                   irelad = as.integer(rep(1,5)),
                   ier = integer(1),
                   llog = integer(1),
                   kmodp = integer(1),
                   maxpd = as.integer(20),
                   nregr = integer(1),
                   kmccde = integer(1),
                   pfail = double(1),
                   npardm = as.integer(5),
                   nnum = integer(1),
                   kparm = integer(1),
                   iup = integer(1),
                   nterd = integer(1))
