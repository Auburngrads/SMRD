star2.mlest <-
function (data.ld, 
          distribution, 
          theta.start = rep(NA, nparm),
          explan.vars = NULL, 
          gamthr = 0, 
          escale = 10000, 
          e = rep(1e-04, nparm), 
          parameter.fixed = NULL, 
          intercept = T, 
          kprint = 0,
          maxit = 500,
          debug1 = F, 
          likelihood.method = GetSMRDDefault("SMRD.likelihood.method"),
          ...)
{
    the.xmat <- xmat(data.ld)
    #if(!is.null(the.xmat)) explan.vars = seq(1:ncol(xmat(data.ld)))
    
    if (!is.null(the.xmat) && is.null(dimnames(the.xmat)[[2]])) {
      
        dimnames(the.xmat) <- list(dimnames(the.xmat)[[1]], 
                                   paste("V", 1:ncol(the.xmat), sep = ""))
        
        warning("No dimnames in xmat --- set to V1,...Vp")
    }
    
    yresp               <- Response(data.ld)
    number.cases        <- nrow(yresp)
    nyresp              <- ncol(yresp)
    the.case.weights    <- case.weights(data.ld)
    the.censor.codes    <- censor.codes(data.ld)
    distribution.number <- numdist(distribution)
    truncation.codes    <- truncation.codes(data.ld)
    tyresp              <- truncation.response(data.ld)
    
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
        
        if (is.null(theta.start)) theta.start <- rep(NA, nparm)
        
        `if`(any(is.na(theta.start)),
             theta.start.comp <- theta.start.est(data.ld, distribution),
             theta.start.comp <- theta.start)
        
      } else {
        
        regression  <- T
        the.xmat    <- Check.xmat(the.xmat, explan.vars, number.cases)
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
        if (is.null(theta.start)) theta.start <- rep(NA, nparm)
        
        `if`(is.null(theta.start) || any(is.na(theta.start)),
             theta.start.comp <- mlest.start.values(data.ld, 
                                                    distribution = distribution,
                                                    intercept = intercept),
             theta.start.comp <- theta.start)
      }
    
    intercept.increment <- as.numeric(intercept || col.of.ones)
    startna <- is.na(theta.start)
    
    if (any(startna)) theta.start[which(startna)] <- theta.start.comp[which(startna)]
    if (map.SMRDDebugLevel() >= 6) {
      
        cat("\ntheta.start.comp", 
            paste(format(theta.start.comp), coll = ","), "\n")
      
        cat("\ntheta.start     ", 
            paste(format(theta.start), coll = ","), "\n")
    
        }
    
    if (length(theta.start) != nparm) {
        
        print(theta.start)
        cat("Wrong number of start values. Should have", nparm, "\n")
        stop("Wrong number of starting values")
        
    }
    
    yresp <- as.matrix(yresp - gamthr)
    mathsoft.gamthr <- rep(0, nrow(yresp))
    relationship <- attr(data.ld, "the.relationships")
    
    if (is.logdist(distribution)) {
      
        non.pos.resp <- yresp[, 1] <= 0
        
        if (any(non.pos.resp)) {
            the.non.pos.resp <- (1:length(non.pos.resp))[non.pos.resp]
            stop(paste("Non positive response values in the following data frame rows:",
                paste(the.non.pos.resp, collapse = ", ")))
            }
        }
    
    if (is.null(parameter.fixed)) parameter.fixed <- rep(F, nparm)
    
    if (regression) {
        is.eyring <- !is.null(relationship) && any(multiple.generic.relationship.name(relationship) ==
            "Eyring")
        if (any(is.eyring)) {
            eyring.relationship <- subscript.relationship(relationship,
                is.eyring)
            if (length(eyring.relationship) > 1)
                stop(paste("More than one Eyring relationship:",
                  paste(relationship, collapse = ", ")))
            eyring.index <- (1:length(is.eyring))[is.eyring] +
                intercept.increment
            tempk <- f.relationshipinv(the.xmat[, eyring.index],
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
    if (generic.distribution(distribution) == "exponential") {
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
    zout <- SMRD2:::WQMMLESSS( ivec = as.integer(ivec), 
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
    
    deviances <- zout$nummat$dev
    #ivec <- zout$intvec$ivec
    #rvec <- zout$numvec$rvec
    log.likelihood <- zout$doubs$xlike
    ierfit <- zout$ints$ierfit
    iervcv <- zout$ints$iervcv
    if (ierfit + iervcv > 0 && options("warn")[[1]] >= 0) {
        warning(paste("MLest warning messages estimation/vcv",
                      ierfit, 
                      iervcv, "
                      \nCheck for extreme outlier or other mismatch between model and data"))
      
        if (T || map.SMRDDebugLevel() >= 4) {
            cat("First derivatives of the loglikelihood = ", paste(zout$numvec$fsder,
                collapse = ","), "\n")
            file.name <- paste("ProblemData", floor(runif(1) * 1e+07), ".ld", sep = "")
            if (map.SMRDDebugLevel() >= 4) {
                assign(envir = .frame0,  inherits = TRUE,file.name, data.ld)
                cat("\nCheck stored data in", file.name, "\n")
            }
            cat("\nStart values\n")
            print(theta.start)
            print(theta.start.comp)
        }
    }
    if (map.SMRDDebugLevel() >= 6) {
        cat("\nStart values after fit\n")
        print(theta.start)
        print(theta.start.comp)
    }
    if(regression && any(is.eyring)) {
      
       eyring.param.index <- nparm - 1
       theta.hat <- zout$numvec$theta[-eyring.param.index]
       first.derivative <- zout$numvec$fsder[-eyring.param.index]
       parameter.fixed <- parameter.fixed[-eyring.param.index]
       vcv.matrix <- matrix(zout$nummat$vcv, ncol = nparm)[-eyring.param.index,-eyring.param.index]
       correlation.matrix <- matrix(zout$nummat$r,ncol = nparm)[-eyring.param.index, -eyring.param.index]
        
     } else {
    
       theta.hat <- zout$numvec$theta
       first.derivative <- zout$numvec$fsder
       vcv.matrix <- zout$nummat$vcv
       correlation.matrix <- zout$nummat$r
       
    }
    names(theta.hat) <- param.names
    names(first.derivative) <- param.names
    names(parameter.fixed) <- param.names
    matnames <- list(param.names, param.names)
    dimnames(correlation.matrix) <- matnames
    dimnames(vcv.matrix) <- matnames
    kodet <- c(rep(1, length(theta.hat) - 1), 2)
    time.units<-attr(data.ld, "time.units")
    
    if(regression) {
      
       fitted.values <- zout$numvec$fv
       residuals <- zout$nummat$res
       the.list <- list(data.ld = data.ld, 
                        distribution = distribution,
                        parameter.fixed = parameter.fixed, 
                        explan.vars = explan.vars,
                        log.likelihood = log.likelihood, 
                        theta.hat = theta.hat,
                        first.derivative = first.derivative, 
                        correlation.matrix = correlation.matrix,
                        vcv.matrix = vcv.matrix, 
                        residuals = residuals, 
                        deviances = deviances,
                        fitted.values = fitted.values, 
                        kodet = kodet, 
                        ierfit = ierfit,
                        iervcv = iervcv, 
                        time.units = time.units)
        
    } else {
      
        the.list <- list(data.ld = data.ld, 
                         distribution = distribution,
                         parameter.fixed = parameter.fixed, 
                         explan.vars = explan.vars,
                         log.likelihood = log.likelihood, 
                         theta.hat = theta.hat,
                         first.derivative = first.derivative, 
                         correlation.matrix = correlation.matrix,
                         vcv.matrix = vcv.matrix, 
                         deviances = deviances, 
                         kodet = kodet,
                         ierfit = ierfit, 
                         iervcv = iervcv, 
                         time.units = time.units)
        
    }
    
    class(the.list) <- c("mlest")
    return(the.list)
    
}
