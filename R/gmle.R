gmle <-
function (data.ld, 
          log.like, 
          theta.start, 
          model = NULL, 
          f.origparam = function(thetatran, model) { theta <- thetatran; return(theta) }, 
          f.tranparam = function(theta, model) { thetatran <- theta; return(thetatran) }, 
          em.alg = NULL, 
          special.stuff = NULL, 
          func.call = match.call(),
          t.param.names = paste("Tparam", seq(1:length(theta.start)), sep = ""),
          orig.param.names = paste("Oparam", seq(1:length(theta.start)), sep = ""),
          max.fcal = 900, 
          max.iter = 300, 
          rfc.tol = 1e-15,
          xc.tol = 1e-13, 
          digits = 5, 
          fixed.param.list = NULL,
          debug1 = 0,
          save.data.ld = T, ...)
{
    data.ld <- data.ld
    log.like <- log.like
    func.call <- match.call()
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    options(digits = digits)
    number.parameters <- length(theta.start)
    fixed.parameters <- fixed.param.list$fixed.parameters
    model$f.tranparam <- f.tranparam
    model$f.origparam <- f.origparam
    t.param.names <- model$t.param.names
    if (is.null(t.param.names)) t.param.names <- paste("thetatran", 1:number.parameters, sep = "")
    orig.param.names <- model$orig.param.names
    if (is.null(orig.param.names)) orig.param.names <- paste("thetaorig", 1:number.parameters, sep = "")
    if (is.null(model$orig.param.names)) model$orig.param.names <- orig.param.names
    if (is.null(model$f.tranparam)) model$f.tranparam <- f.tranparam
    assign(envir = .frame0,  inherits = TRUE,"log.like", log.like)
    assign(envir = .frame0,  inherits = TRUE,"model", model)
    if (!exists("debug1")) assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    
    if (map.SMRDDebugLevel() >= 4) {
      
       debug1<- get(envir = .frame0, "debug1")
        if (debug1 < 3) assign(envir = .frame0,  inherits = TRUE,"debug1", 3)
    }
    
    assign(envir = .frame0,  inherits = TRUE,"data.ld", value = unfold(data.ld))
    assign(envir = .frame0,  inherits = TRUE,"special.stuff", special.stuff)
    if (!is.null(attr(data.ld, "sim.parameters"))) theta.start <- attr(data.ld, "sim.parameters")
    
    if (is.null(em.alg)) {
      
        if (!is.null(fixed.param.list)) {
          
            fixed.parameter.values <- fixed.param.list$fixed.parameter.values
            theta.start[fixed.parameters] <- fixed.parameter.values
            assign(envir = .frame0,  inherits = TRUE,"profile.on", value = fixed.parameters)
            assign(envir = .frame0,  inherits = TRUE,"profile.on.pos", value = fixed.parameters)
            
            profile.stable.parameters <- function(x.theta.hat, profile.on) {
                theta.hat <- x.theta.hat
                return(theta.hat)
            }
            
            assign(envir = .frame0,  inherits = TRUE,"profile.stable.parameters", value = profile.stable.parameters)
            theta.start <- f.tranparam(theta.start, model)
            assign(envir = .frame0,  inherits = TRUE,"theta.hold", value = theta.start)
            theta.opt <- theta.start[.Uminus(fixed.parameters)]
            est.out <- wqm.nlmin(const.log.like, 
                                 theta.opt, 
                                 max.fcal = max.fcal,
                                 max.iter = max.iter, 
                                 rfc.tol = rfc.tol, 
                                 xc.tol = xc.tol)
            t.param <- est.out$x
            grad <- my.gradient(const.log.like, t.param)
            hessian <- my.hessian(const.log.like, t.param)
      } else {
            theta.start <- f.tranparam(theta.start, model)
            est.out <- wqm.nlmin(log.like, 
                                 theta.start, 
                                 max.fcal = max.fcal,
                                 max.iter = max.iter, 
                                 rfc.tol = rfc.tol, 
                                 xc.tol = xc.tol)
            t.param <- est.out$x
            grad <- my.gradient(log.like, t.param)
            hessian <- my.hessian(log.like, t.param)
        }
  } else {
        est.out <- em.alg(data.ld, theta.start)
        grad <- my.gradient(log.like, t.param)
        hessian <- my.hessian(log.like, t.param)
    }

    `if`(!is.null(fixed.param.list),
         max.log.like <- .Uminus(const.log.like(t.param)),
         max.log.like <- .Uminus(log.like(t.param)))
    
    ierror <- 0
    
    if (is.infinite(max.log.like) || any(is.nan(hessian)) ||
        any(is.na(hessian))) {
        ierror <- 1
        warning("bad hessian values")
        print(hessian)
        t.vcv <- hessian
        eigen.hessian <- NA
  } else {
        eigen.hessian <- eigen(hessian)
        if (any(eigen.hessian$values <= 0)) {
            ierror <- 1
            warning("Nonpositive eigenvalues")
                   }
        t.vcv <- my.solve(hessian)
    }
    if (debug1 > 2 && ierror > 0) {
        cat("\nConvergence stuff\n")
        print(est.out)
        cat("\nGradient\n")
        print(grad)
        cat("\nLikelihood\n")
        print(max.log.like)
        cat("\nHessian\n")
        print(hessian)
    }
    hessian <- matrix.filler(hessian, fixed.parameters, NA)
    grad <- matrix.filler(grad, fixed.parameters, NA)
    t.param <- matrix.filler(t.param, fixed.parameters, theta.start)
    t.vcv <- matrix.filler(t.vcv, fixed.parameters, 0)
    names(t.param) <- t.param.names
    names(grad) <- t.param.names
    dimnames(hessian) <- list(t.param.names, t.param.names)
    dimnames(t.vcv) <- list(t.param.names, t.param.names)
    t.standard.error <- sqrt(abs(diag(t.vcv)))
    names(t.param.names) <- t.param.names
    names(t.standard.error) <- t.param.names
    
    gmle.out <- list(data.ld = data.ld, 
                     log.like = log.like,
                     model = model, 
                     em.alg = em.alg, 
                     special.stuff = special.stuff,
                     est.out = est.out,
                     max.log.like = max.log.like, 
                     grad = grad,
                     hessian = hessian, 
                     eigen.hessian = eigen.hessian,
                     t.vcv = t.vcv,
                     t.param = t.param, 
                     t.standard.error = t.standard.error,
                     fixed.param.list = fixed.param.list, 
                     ierror = ierror)
    
    if (!is.null(f.origparam)) {
      
        genorigparm.out <- f.genorigparmvcv(t.vcv, 
                                            t.param, 
                                            f.origparam,
                                            model = model)
        origparam <- genorigparm.out$vec
        if (is.null(names(origparam))) names(origparam) <- model$orig.param.names
        gmle.out$origparam <- origparam
        origparamvcv <- genorigparm.out$vcv
        dimnames(origparamvcv) <- list(model$orig.param.names,
                                       model$orig.param.names)
        gmle.out$origparamvcv <- origparamvcv
        
    }
    gmle.out$date <- date()
    if (!save.data.ld) gmle.out$data.ld <- NULL
    oldClass(gmle.out) <- "gmle.out"
    return(gmle.out)
}
