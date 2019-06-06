library(smrdfortran)
library(SMRD)
test = 6
if(test == 1){
  
   data.ld <- frame.to.ld(lzbearing, response.column = 1) 
   
}
if(test == 2){
  
   data.ld <- frame.to.ld(heatexchanger, 
                          response.column = c(1,2),
                          censor.column = 3,
                          case.weight.column = 4) 
}
if(test == 3){
  
   data.ld <- frame.to.ld(prob3_5, 
                          response.column = 1,
                          censor.column = 2)
}
if(test == 4){
  
  data.ld <- frame.to.ld(berkson200,
                         response.column = c(1,2),
                         censor.column = 3,
                         case.weight.column = 4,
                         time.units = "1/5000 Seconds")
}
if(test == 5){
  
  data.ld <- frame.to.ld(bulb,
                       response.column = 1,
                       data.title = "Bulb Data",
                       time.units = "Hours")
  
}
if(test == 6){
  
  data.ld <- frame.to.ld(bearingcage,
                         response.column = 1, 
                         censor.column = 2, 
                         case.weight.column = 3,
                         time.units = "Hours")
}

distribution = 'lognormal' 
explan.vars = NULL
gamthr = 0
escale = 10000 
parameter.fixed = NULL 
intercept = T
if(!exists("kprint")) kprint = 0
maxit = 500
debug1 = F 
likelihood.method = SMRD:::GetsmrdfortranDefault("smrdfortran.likelihood.method")


    the.xmat <- SMRD:::xmat(data.ld)
    #if(!is.null(the.xmat)) explan.vars = seq(1:ncol(xmat(data.ld)))
    
    if (!is.null(the.xmat) && is.null(dimnames(the.xmat)[[2]])) {
      
        dimnames(the.xmat) <- list(dimnames(the.xmat)[[1]], 
                                   paste("V", 1:ncol(the.xmat), sep = ""))
        
        warning("No dimnames in xmat --- set to V1,...Vp")
    }
    
    yresp               <- SMRD:::Response(data.ld)
    number.cases        <- nrow(yresp)
    nyresp              <- ncol(yresp)
    the.case.weights    <- SMRD:::case.weights(data.ld)
    the.censor.codes    <- SMRD:::censor.codes(data.ld)
    distribution.number <- SMRD:::numdist(distribution)
    truncation.codes    <- SMRD:::truncation.codes(data.ld)
    tyresp              <- SMRD:::truncation.response(data.ld)
    
    if (!is.null(truncation.codes) && !is.null(tyresp)) {
      
        ntyresp <- ncol(tyresp)
        
    } else {
  
        ntyresp <- 0
        truncation.codes <- rep(1, length(the.censor.codes))
        tyresp <- rep(0, length(the.censor.codes))
    }
    
    if (length(gamthr) == 1) gamthr <- rep(gamthr, number.cases)
    if (length(gamthr) != number.cases) stop("specified offset is the wrong length")
    
    if (is.null(explan.vars)) {
        nter        <- 1
        nparm       <- nter + 1
        regression  <- F
        int         <- 1
        the.xmat    <- cbind(rep(1, number.cases))
        param.names <- c("mu", "sigma")
        theta.start = rep(NA, nparm)
        
        if (is.null(theta.start)) theta.start <- rep(NA, nparm)
        
        `if`(any(is.na(theta.start)),
             theta.start.comp <- SMRD:::theta.start.est(data.ld, distribution),
             theta.start.comp <- theta.start)
        
      } else {
        
        regression  <- T
        the.xmat    <- SMRD:::Check.xmat(the.xmat, explan.vars, number.cases)
        RegrNames   <- dimnames(the.xmat)[[2]][explan.vars]
        col.of.ones <- all(abs(the.xmat[, 1] - 1) < 1e-10)
        
        `if`(intercept && !col.of.ones,
             { int <- 1
               the.xmat <- cbind(rep(1, number.cases), 
                                 the.xmat[, explan.vars, drop = F])
               param.names <- c("b0", RegrNames, "sigma") },
             { int <- 0
               the.xmat <- the.xmat[, explan.vars, drop = F]
               param.names <- c(RegrNames, "sigma")})
        
        nter <- ncol(the.xmat)
        nparm <- nter + 1
        theta.start = rep(NA, nparm)
        if (is.null(theta.start)) theta.start <- rep(NA, nparm)
        
        `if`(is.null(theta.start) || any(is.na(theta.start)),
             theta.start.comp <- SMRD:::mlest.start.values(data.ld, 
                                                    distribution = distribution,
                                                    intercept = intercept),
             theta.start.comp <- theta.start)
      }
    

e = rep(1e-04, nparm) 
intercept.increment <- as.numeric(intercept || col.of.ones)
startna <- is.na(theta.start)
    
if (any(startna)) theta.start[startna] <- theta.start.comp[startna]

    
    if (length(theta.start) != nparm) {
        
      print(theta.start)
        cat("Wrong number of start values. Should have", 
            nparm, "\n")
        stop("Wrong number of starting values")
        
    }
    
    yresp <- as.matrix(yresp - gamthr)
    mathsoft.gamthr <- rep(0, nrow(yresp))
    relationship <- attr(data.ld, "the.relationships")
    
    if (SMRD:::is.logdist(distribution)) {
      
        non.pos.resp <- yresp[, 1] <= 0
        
        if (any(non.pos.resp)) {
            the.non.pos.resp <- (1:length(non.pos.resp))[non.pos.resp]
            stop(paste("Non positive response values in the following data frame rows:",
                paste(the.non.pos.resp, collapse = ", ")))
            }
        }
    
    if (is.null(parameter.fixed)) parameter.fixed <- rep(F, nparm)
    
    if (regression) {
        is.eyring <- !is.null(relationship) && any(SMRD:::multiple.generic.relationship.name(relationship) ==
            "Eyring")
        if (any(is.eyring)) {
            eyring.relationship <- SMRD:::subscript.relationship(relationship,
                is.eyring)
            if (length(eyring.relationship) > 1)
                stop(paste("More than one Eyring relationship:",
                  paste(relationship, collapse = ", ")))
            eyring.index <- (1:length(is.eyring))[is.eyring] +
                intercept.increment
            tempk <- SMRD:::f.relationshipinv(the.xmat[, eyring.index],
                eyring.relationship) + 273.16
            eyring.power <- attr(relationship, "the.power")[is.eyring]
            correction <- -log(tempk) * eyring.power
            the.xmat <- cbind(the.xmat, correction)
            theta.start <- c(theta.start[-nparm], 1, theta.start[nparm])
            parameter.fixed <- c(parameter.fixed[-nparm], T,
                parameter.fixed[nparm])
            nparm <- nparm + 1
            nter <- nter + 1
        }
    }
    dummy <- the.censor.codes
    if (SMRD:::generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        theta.start[nparm] <- 1
        parameter.fixed[nparm] <- T
    }
    ndscrat <- nparm * number.cases + 5 * nparm * nparm + 12 * nparm + 1
    niscrat <- 2 * (nparm + 1)
    
    ivec <- c(number.case = number.cases, 
              nter = nter, 
              nyresp = nyresp,
              ntyresp = ntyresp, 
              distribution.number = distribution.number,
              lcheck = 0, 
              nparm = nparm, 
              int = int, 
              maxit = maxit,
              kprint = kprint, 
              ierfit = 0, 
              iervcv = 0)
    
    rvec <- c(0, escale = escale, log.likelihood = 0)
    zout <- .Fortran("wqmmlesss",
                     ivec = as.integer(ivec),
                     rvec = as.single(rvec),
                     number.cases = as.integer(number.cases),
                     nparm = as.integer(nparm),
                     xmat = as.single(the.xmat),
                     yresp = as.single(yresp),
                     as.single(c(the.censor.codes, the.case.weights, mathsoft.gamthr)),
                     tyresp = as.single(tyresp),
                     truncation.codes = as.single(truncation.codes),
                     parameter.fixed = as.logical(parameter.fixed),
                     e = as.single(e),
                     ndscrat = double(ndscrat),
                     niscrat = integer(niscrat),
                     theta.hat = as.single(theta.start),
                     first.derivative = single(nparm),
                     vcv.matrix = single(nparm * nparm),
                     correlation.matrix = single(nparm * nparm),
                     residuals = single(nyresp * number.cases),
                     fitted.values.and.deviance = single(4 * number.cases))

new = SMRD:::WQMMLESSS(  ivec = as.integer(ivec), 
                      rvec = as.double(rvec),
                      nrow = as.integer(number.cases), 
                      nparm = as.integer(nparm),
                      x = the.xmat, 
                      y = yresp,
                      cen = as.double(the.censor.codes), 
                      wt =  as.double(the.case.weights), 
                      msftgm = as.double(mathsoft.gamthr),
                      ty = as.matrix(tyresp), 
                      tcodes = as.double(truncation.codes),
                      lfix = as.logical(parameter.fixed), 
                      e = as.double(e),
                      dscrat = double(ndscrat), 
                      iscrat = integer(niscrat),
                      theta = as.double(theta.start), 
                      fsder = double(nparm),
                      vcv = matrix(0, nrow = nparm, ncol = nparm), 
                      r = matrix(0, nrow = nparm, ncol = nparm), 
                      res = matrix(0, nrow = number.cases, ncol = nyresp),
                      fv = double(number.cases),
                      dev = matrix(0, nrow = number.cases, ncol = 3),
                      ipxnew = matrix(0, nrow = number.cases, ncol = nter),
                      iprv1 = double(nparm),
                      ipdiag = double(nparm),
                      iptmat = matrix(0, nrow = nparm, ncol = nparm),
                      ipthb = double(nparm),
                      ipthg = double(nparm),
                      ipfsd = double(nparm),
                      ipvcvb = matrix(0, nrow = nparm, ncol = nparm),
                      ipvcvg = matrix(0, nrow = nparm, ncol = nparm),
                      ipnext = double(nparm),
                      itd = double(nparm),
                      itf = double(nparm),
                      ied = double(nparm),
                      iw = double(nparm * nparm + 3 * nparm),
                      ivd = double(nparm),
                      ivcvd = matrix(0, nrow = nparm, ncol = nparm),
                      ivcvdd = matrix(0, nrow = nparm + 1, ncol = nparm + 1),
                      iir = double(nparm + 1),
                      ijc = double(nparm + 1))
#cat("xnew"); xnew.end = 0;
#             zout$ndscrat[(xnew.end + 1):(number.cases*nter)] - new$nummat$ipxnew[1:(number.cases*nter)]
#cat("rv1") ; rv1.end = xnew.end + number.cases*nter
#             zout$ndscrat[(rv1.end + 1):(rv1.end + nparm)] - new$numvec$iprv1
#cat("diag"); diag.end = rv1.end + nparm
#             zout$ndscrat[(diag.end + 1):(diag.end + nparm)] - new$numvec$ipdiag
#cat("tmat"); tmat.end = diag.end + nparm
#             zout$ndscrat[(tmat.end + 1):(tmat.end + nparm * 2)] - new$nummat$iptmat[1:(nparm * 2)]
#cat("thetb"); thetb.end = tmat.end + nparm * 2
#              zout$ndscrat[(thetb.end + 1):(thetb.end + nparm)] - new$numvec$ipthb
#cat("thetg"); thetg.end = thetb.end + nparm
#              zout$ndscrat[(thetg.end + 1):(thetg.end + nparm)] - new$numvec$ipthg
#cat("fsder"); fsder.end = thetg.end + nparm
#              zout$ndscrat[(fsder.end + 1):(fsder.end + nparm)] - new$numvec$fsder
#cat("vcv")  ; vcv.end = fsder.end + nparm
#              zout$ndscrat[(vcv.end + 1):(vcv.end + nparm ** 2)] - new$nummat$vcv[1:(nparm ** 2)]
#cat("vcvg") ; vcvg.end = vcv.end + nparm ** 2
#              zout$ndscrat[(vcvg.end + 1):(vcvg.end + nparm ** 2)] - new$nummat$ipvcvg[1:(nparm ** 2)]
#cat("itd");  itd.end = vcvg.end + nparm ** 2
#             zout$ndscrat[(itd.end + 1):(itd.end + nparm)] - new$numvec$itd
#cat("itf");  itf.end = itd.end + nparm
#             zout$ndscrat[(itf.end + 1):(itf.end + nparm)] - new$numvec$itf
#cat("ied");  ied.end = itf.end + nparm
#             zout$ndscrat[(ied.end + 1):(ied.end + nparm)] - new$numvec$ied
#cat("iw");  iw.end = ied.end + nparm
#            zout$ndscrat[(iw.end + 1):(iw.end + nparm*nparm+3*nparm)] - new$numvec$iw
#cat("ivd") ;  ivd.end = iw.end + nparm*nparm+3*nparm
#              zout$ndscrat[(ivd.end + 1):(ivd.end + nparm)] - new$numvec$ivd
#cat("ivcvd");  ivcvd.end = ivd.end + nparm
#               zout$ndscrat[(ivcvd.end + 1):(ivcvd.end + nparm ** 2)] - new$nummat$ivcvd[1:(nparm ** 2)]
#cat("ivcvdd"); ivcvdd.end = ivcvd.end + nparm ** 2
#               zout$ndscrat[(ivcvdd.end + 1):(ivcvdd.end + (nparm + 1) ** 2)] - new$nummat$ivcvdd[1:((nparm + 1) ** 2)]
#cat("theta"); zout$theta - new$numvec$theta
#cat("devian"); zout$fitted[-(1:number.cases)] - new$nummat$dev
#cat("fitted"); zout$fitted[1:number.cases] - new$numvec$fv
#cat("resids"); zout$residuals - new$nummat$res
#cat("correl"); zout$correl - new$nummat$r
#cat("yresp");  zout$yresp - new$nummat$y
#cat("xlike");  zout$rvec[3] - new$doubs$xlike
#cat("ierfit");  zout$ivec[11] - new$ints$ierfit
#cat("iervcv");  zout$ivec[12] - new$ints$iervcv
