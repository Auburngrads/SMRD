parametric.bootstrap <-
  function (data.ld, 
            distribution, 
            number.sim, 
            escale = 10000, 
            e = rep(1e-04, number.parameters), 
            parameter.fixed = rep(F, number.parameters), 
            intercept = T, 
            kprint = 0, 
            maxit = 500, 
            max.sim.scratch.space = 1000, 
            debug1 = F, 
            randomize = T) 
  {
    
    `if`(randomize,
         tspass <- runif(33),
         tspass <- seq(0.1, 0.4, length = 33))
    
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    nty <- 0
    nter <- 1
    int <- 1
    mlest.out <- mlest(data.ld, 
                       distribution = distribution, 
                       kprint = kprint)
    
    theta.start <- mlest.out$theta.hat
    theta.hat <- theta.start
    distribution.number <- numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    
    if (generic.distribution(distribution) == "exponential") {
      
       distribution.number <- 2
       parameter.fixed[number.parameters] <- T
       number.parametersx <- 1
       
    }
    iret <- 3
    number.things.returned <- number.parameters + ((number.parameters) * 
                                                     (number.parameters + 1))/2 + 2
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    the.xmat <- matrix(1, nrow = number.cases, ncol = 1)
    ndscrat <- number.parameters * 
               number.cases + 5 * 
               number.parameters *
               number.parameters + 12 * 
               number.parameters + 1
    niscrat <- 2 * (number.parameters + 1)
    sim.scratch.space <- min(sum(the.case.weights), 
                             max(max.sim.scratch.space, number.cases))
    
    if (debug1) browser()
    
    zout <- .Fortran("mlsim2", 
                     as.single(the.xmat), 
                     as.single(y), 
                     as.single(the.censor.codes), 
                     as.single(the.case.weights), 
                     as.integer(number.cases), 
                     as.integer(nter), 
                     as.integer(ny), 
                     as.integer(nty), 
                     ty = single(number.cases), 
                     tc = single(number.cases), 
                     distribution.number = as.integer(distribution.number), 
                     gamthr = single(number.cases), 
                     parameter.fixed = as.logical(parameter.fixed), 
                     number.parameters = as.integer(number.parameters), int = as.integer(int), 
                     escale = as.single(escale), 
                     e = as.single(e), 
                     maxit = as.integer(maxit), 
                     kprint = as.integer(kprint), 
                     dscrat = double(ndscrat), 
                     iscrat = integer(niscrat), 
                     devian = single(number.cases * 3), 
                     thetah = as.single(theta.start), 
                     first.derivative = single(number.parameters), 
                     vcv.matrix = single(number.parameters * number.parameters), 
                     correlation.matrix = single(number.parameters * number.parameters), 
                     residuals = single(ny * number.cases), 
                     fitted.values = single(ny * number.cases), 
                     theta = single(number.parameters), 
                     iarray = integer(sim.scratch.space), 
                     marray = as.integer(sim.scratch.space), 
                     wtnew = single(number.cases), 
                     xnew = single(number.cases * nter), 
                     ynew = single(number.cases * ny), 
                     iret = as.integer(iret), 
                     return.matrix = single(number.sim * number.things.returned), 
                     number.sim = as.integer(number.sim), 
                     numret = as.integer(number.things.returned), 
                     tspass = as.single(tspass), 
                     lrand = as.logical(randomize), 
                     iersim = integer(1))
    
    if (zout$iersim > 0 || debug1) {
      browser()
      if (zout$iersim > 0) stop("Need more space for observations")
    }
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    theta.hat.star <- return.matrix[, 2:(number.parameters + 
                                           1)]
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    vcv <- return.matrix[, (number.parameters + 3):(number.parameters + 
                                                      ((number.parameters) * (number.parameters + 1))/2 + 2)]
    if (generic.distribution(distribution) == "exponential") {
      theta.hat.star[, 1] <- exp(theta.hat.star[, 1])
      theta.hat[1] <- exp(theta.hat[1])
      mlest.out$theta.hat <- theta.hat
      mlest.out$vcv.matrix[1, 1] <- mlest.out$vcv.matrix[1, 
                                                         1] * theta.hat[1]^2
      vcv[, 1] <- vcv[, 1] * ((theta.hat.star[, 1])^2)
      names(mlest.out$theta.hat)[1] <- "Theta"
    }
    the.results <- list(theta.hat = theta.hat, 
                        theta.hat.star = theta.hat.star, 
                        ierstuff = ierstuff, 
                        likelihood = return.matrix[, number.parameters + 2], 
                        vcv = vcv, 
                        mlest.out = mlest.out)
    
    oldClass(the.results) <- "boot.npar.par.out"
    return(the.results)
    
  }

#'
#'

nonparametric.bootstrap <-
  function (data.ld, number.sim = 10, kprint = 0, maxit = 500, 
            max.sim.scratch.space = 1000, maxmsd = 100,debug1= F, randomize = T) 
  {
    if (randomize) {
      tspass <- runif(33)
    }
    else {
      tspass <- seq(0.1, 0.4, length = 33)
    }
    nty <- 0
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    ndscrat <- 3 * number.cases + 4
    niscrat <- 6 * number.cases + 7
    nrscrat <- max(7 * (number.cases + 1), (maxmsd * (maxmsd - 
                                                        1))/2 + 1)
    sim.scratch.space <- min(sum(the.case.weights), max(max.sim.scratch.space, 
                                                        number.cases))
    cdfest.out <- cdfest(data.ld)
    m <- length(cdfest.out$p)
    number.things.returned <- 2 * m + 1
    if (debug1) 
      browser()
    zout <- .Fortran("mlsim3", as.single(y), as.single(the.censor.codes), 
                     as.single(the.case.weights), as.integer(number.cases), 
                     as.integer(ny), as.integer(nty), ty = single(number.cases), 
                     tc = single(number.cases), gamthr = single(number.cases), 
                     as.integer(maxit), as.integer(kprint), double(ndscrat), 
                     integer(niscrat), single(nrscrat), as.single(cdfest.out$p), 
                     as.single(cdfest.out$q), single(m), single(m), as.integer(m), 
                     single(m), single(m), single(m), single(m), integer(sim.scratch.space), 
                     as.integer(sim.scratch.space), wtnew = single(number.cases), 
                     ynew = single(number.cases * ny), return.matrix = single(number.sim * 
                                                                                number.things.returned), as.integer(number.sim), 
                     as.integer(number.things.returned), as.single(tspass), 
                     as.logical(randomize), iersim = integer(1))
    if (zout$iersim > 0 || debug1) {
      browser()
      if (zout$iersim > 0) 
        stop("Need more space for observations")
    }
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    f.hat.star <- return.matrix[, 2:(m + 1)]
    standard.errors <- return.matrix[, (m + 2):(2 * m + 1)]
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    the.results <- list(cdfest.out = cdfest.out, p = cdfest.out$p, 
                        q = cdfest.out$q, f.hat = cdfest.out$prob, sd = cdfest.out$sd, 
                        f.hat.star = f.hat.star, stderror.star = standard.errors, 
                        ierstuff = ierstuff)
    oldClass(the.results) <- "boot.npar.npar.out"
    return(the.results)
  }

#'
#'

boot.npar.npar <-
  function (data.ld, number.sim = 10, kprint = 0, maxit = 500, 
            max.sim.scratch.space = 1000, maxmsd = 100,debug1= F, randomize = T) 
  {
    if (randomize) {
      tspass <- runif(33)
    }
    else {
      tspass <- seq(0.1, 0.4, length = 33)
    }
    nty <- 0
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    ndscrat <- 3 * number.cases + 4
    niscrat <- 6 * number.cases + 7
    nrscrat <- max(7 * (number.cases + 1), (maxmsd * (maxmsd - 
                                                        1))/2 + 1)
    sim.scratch.space <- min(sum(the.case.weights), max(max.sim.scratch.space, 
                                                        number.cases))
    cdfest.out <- cdfest(data.ld)
    m <- length(cdfest.out$p)
    number.things.returned <- 2 * m + 1
    if (debug1) 
      browser()
    zout <- .Fortran("mlsim3", as.single(y), as.single(the.censor.codes), 
                     as.single(the.case.weights), as.integer(number.cases), 
                     as.integer(ny), as.integer(nty), ty = single(number.cases), 
                     tc = single(number.cases), gamthr = single(number.cases), 
                     as.integer(maxit), as.integer(kprint), double(ndscrat), 
                     integer(niscrat), single(nrscrat), as.single(cdfest.out$p), 
                     as.single(cdfest.out$q), single(m), single(m), as.integer(m), 
                     single(m), single(m), single(m), single(m), integer(sim.scratch.space), 
                     as.integer(sim.scratch.space), wtnew = single(number.cases), 
                     ynew = single(number.cases * ny), return.matrix = single(number.sim * 
                                                                                number.things.returned), as.integer(number.sim), 
                     as.integer(number.things.returned), as.single(tspass), 
                     as.logical(randomize), iersim = integer(1))
    if (zout$iersim > 0 || debug1) {
      browser()
      if (zout$iersim > 0) 
        stop("Need more space for observations")
    }
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    f.hat.star <- return.matrix[, 2:(m + 1)]
    standard.errors <- return.matrix[, (m + 2):(2 * m + 1)]
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    the.results <- list(cdfest.out = cdfest.out, p = cdfest.out$p, 
                        q = cdfest.out$q, f.hat = cdfest.out$prob, sd = cdfest.out$sd, 
                        f.hat.star = f.hat.star, stderror.star = standard.errors, 
                        ierstuff = ierstuff)
    oldClass(the.results) <- "boot.npar.npar.out"
    return(the.results)
  }

#'
#'

boot.npar.par <-
  function (data.ld, distribution, number.sim, escale = 10000, 
            e = rep(1e-04, number.parameters), parameter.fixed = rep(F, 
                                                                     number.parameters), intercept = T, kprint = 0, maxit = 500, 
            max.sim.scratch.space = 1000,debug1= F, randomize = T) 
  {
    if (randomize) {
      tspass <- runif(33)
    } else {
      tspass <- seq(0.1, 0.4, length = 33)
    }
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    nty <- 0
    nter <- 1
    int <- 1
    mlest.out <- mlest(data.ld, distribution = distribution, 
                       kprint = kprint)
    theta.start <- mlest.out$theta.hat
    theta.hat <- theta.start
    distribution.number <- numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    if (generic.distribution(distribution) == "exponential") {
      distribution.number <- 2
      parameter.fixed[number.parameters] <- T
      number.parametersx <- 1
    }
    iret <- 3
    number.things.returned <- number.parameters + ((number.parameters) * 
                                                     (number.parameters + 1))/2 + 2
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    the.xmat <- matrix(1, nrow = number.cases, ncol = 1)
    ndscrat <- number.parameters * number.cases + 5 * number.parameters * 
      number.parameters + 12 * number.parameters + 1
    niscrat <- 2 * (number.parameters + 1)
    sim.scratch.space <- min(sum(the.case.weights), max(max.sim.scratch.space, 
                                                        number.cases))
    if (debug1) 
      browser()
    zout <- .Fortran("mlsim2", as.single(the.xmat), as.single(y), 
                     as.single(the.censor.codes), as.single(the.case.weights), 
                     as.integer(number.cases), as.integer(nter), as.integer(ny), 
                     as.integer(nty), ty = single(number.cases), tc = single(number.cases), 
                     distribution.number = as.integer(distribution.number), 
                     gamthr = single(number.cases), parameter.fixed = as.logical(parameter.fixed), 
                     number.parameters = as.integer(number.parameters), int = as.integer(int), 
                     escale = as.single(escale), e = as.single(e), maxit = as.integer(maxit), 
                     kprint = as.integer(kprint), dscrat = double(ndscrat), 
                     iscrat = integer(niscrat), devian = single(number.cases * 
                                                                  3), thetah = as.single(theta.start), first.derivative = single(number.parameters), 
                     vcv.matrix = single(number.parameters * number.parameters), 
                     correlation.matrix = single(number.parameters * number.parameters), 
                     residuals = single(ny * number.cases), fitted.values = single(ny * 
                                                                                     number.cases), theta = single(number.parameters), 
                     iarray = integer(sim.scratch.space), marray = as.integer(sim.scratch.space), 
                     wtnew = single(number.cases), xnew = single(number.cases * 
                                                                   nter), ynew = single(number.cases * ny), iret = as.integer(iret), 
                     return.matrix = single(number.sim * number.things.returned), 
                     number.sim = as.integer(number.sim), numret = as.integer(number.things.returned), 
                     tspass = as.single(tspass), lrand = as.logical(randomize), 
                     iersim = integer(1))
    if (zout$iersim > 0 || debug1) {
      browser()
      if (zout$iersim > 0) 
        stop("Need more space for observations")
    }
    return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
    theta.hat.star <- return.matrix[, 2:(number.parameters + 
                                           1)]
    ierstuff <- as.integer(return.matrix[, 1] + 0.1)
    vcv <- return.matrix[, (number.parameters + 3):(number.parameters + 
                                                      ((number.parameters) * (number.parameters + 1))/2 + 2)]
    if (generic.distribution(distribution) == "exponential") {
      theta.hat.star[, 1] <- exp(theta.hat.star[, 1])
      theta.hat[1] <- exp(theta.hat[1])
      mlest.out$theta.hat <- theta.hat
      mlest.out$vcv.matrix[1, 1] <- mlest.out$vcv.matrix[1, 
                                                         1] * theta.hat[1]^2
      vcv[, 1] <- vcv[, 1] * ((theta.hat.star[, 1])^2)
      names(mlest.out$theta.hat)[1] <- "Theta"
    }
    the.results <- list(theta.hat = theta.hat, theta.hat.star = theta.hat.star, 
                        ierstuff = ierstuff, likelihood = return.matrix[, number.parameters + 
                                                                          2], vcv = vcv, mlest.out = mlest.out)
    oldClass(the.results) <- "boot.npar.par.out"
    return(the.results)
  }

#'
#'

compare.summary.boot.npar.npar.out <-
  function (mlesim.out, time.index = last.one, my.title = NULL,
            original.par = T, which.ones = c("percentile", "boott", "boott.logit"),
            conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, cum.method = "boott.logit")
  {
    old.par <- par(mfrow = c(2, 2))
    if (original.par)
      on.exit(par(old.par))
    last.one <- length(mlesim.out$p)
    return.list <- NULL
    if (!is.na(match("percentile", which.ones))) {
      percentile <- summary.boot.npar.npar.out(mlesim.out,
                                               time.index = time.index, method = "percentile", my.title = my.title,
                                               conf.level = conf.level, cum.method = cum.method)
      return.list$percentile <- percentile
    }
    if (!is.na(match("boott", which.ones))) {
      boott <- summary.boot.npar.npar.out(mlesim.out, time.index = time.index,
                                          method = "boott", my.title = my.title, conf.level = conf.level,
                                          cum.method = cum.method)
      return.list$boott <- boott
    }
    if (!is.na(match("boott.logit", which.ones))) {
      boott.logit <- summary.boot.npar.npar.out(mlesim.out,
                                                time.index = time.index, method = "boott.logit",
                                                my.title = my.title, conf.level = conf.level, cum.method = cum.method)
      return.list$boott.logit <- boott.logit
    }
    if (!is.na(match("boott.asin", which.ones))) {
      boott.asin <- summary.boot.npar.npar.out(mlesim.out,
                                               time.index = time.index, method = "boott.asin", my.title = my.title,
                                               conf.level = conf.level, cum.method = cum.method)
      return.list$boott.asin <- boott.asin
    }
    return(return.list)
  }

#'
#'

compare.summary.boot.npar.par.out <-
  function (boot.par.out, inference.on = "parameter", which = 1,
            parameter.name = NULL, my.title = NULL, original.par = T,
            conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, which.ones = NULL,
            cum.method = NULL, cex.labs = 1.1, cex.title = 0.8)
  {
    old.par <- par(mfrow = c(2, 2), mar = c(4.5, 4.1, 3.5, 2.1))
    if (original.par)
      on.exit(par(old.par))
    last.one <- length(boot.par.out$p)
    trans.method <- which.boot.method.to.use(inference.on, which,
                                             mlest.out = boot.par.out$mlest)
    which.ones <- unique(c("percentile", "boott", trans.method))
    return.list <- NULL
    if (is.element("percentile", which.ones)) {
      percentile <- focus.boot.npar.par.out(boot.par.out, inference.on = inference.on,
                                            which = which, method = "percentile", my.title = "",
                                            conf.level = conf.level, parameter.name = parameter.name,
                                            cum.method = NULL, cex.title = cex.title, cex.labs = cex.labs)
      return.list$percentile <- percentile
    }
    if (is.element("boott", which.ones)) {
      if (trans.method == "boott.notran")
        cum.method <- "boott.notran"
      else cum.method <- NULL
      boott <- focus.boot.npar.par.out(boot.par.out, inference.on = inference.on,
                                       which = which, method = "boott.notran", my.title = "",
                                       conf.level = conf.level, parameter.name = parameter.name,
                                       cum.method = cum.method, cex.title = cex.title, cex.labs = cex.labs)
      return.list$boott <- boott
    }
    if (is.element("boott.log", which.ones)) {
      if (trans.method == "boott.log")
        cum.method <- "boott.log"
      else cum.method <- NULL
      boottlog <- focus.boot.npar.par.out(boot.par.out, inference.on = inference.on,
                                          which = which, method = "boott.log", my.title = "",
                                          conf.level = conf.level, parameter.name = parameter.name,
                                          cum.method = cum.method, cex.title = cex.title, cex.labs = cex.labs)
      return.list$boottlog <- boottlog
    }
    if (is.element("boott.logit", which.ones)) {
      if (trans.method == "boott.logit")
        cum.method <- "boott.logit"
      else cum.method <- NULL
      boottlogit <- focus.boot.npar.par.out(boot.par.out, inference.on = inference.on,
                                            which = which, method = "boott.logit", my.title = "",
                                            conf.level = conf.level, parameter.name = parameter.name,
                                            cum.method = cum.method, cex.title = cex.title, cex.labs = cex.labs)
      return.list$boottlogit <- boottlogit
    }
    if (is.null(my.title))
      my.title <- get.data.title(boot.par.out$mlest.out$data.ld)
    #mtext(side = 3, line = -1.4, text = my.title, outer = T, cex = 1.4)
    invisible(return.list)
  }

#'
#'

focus.boot.npar.par.out <-
  function (boot.par.out, inference.on = "parameter", which = 1,
            method = NULL, parameter.name = NULL, my.title = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            cum.method = NULL, cex.title = 1, cex.labs = 1.1)
  {
    
    wqm.hist <-
      function (x, breaks = "Sturges", freq = NULL, include.lowest = TRUE,
                right = TRUE, density = NULL, angle = 45, col = NULL, border = "darkgrey",
                main = "", xlim = NULL, ylim = NULL, xlab = deparse(substitute(x)),
                ylab = "Frequency", axes = TRUE, plot = TRUE, labels = FALSE,
                nclass = NULL, trim = F, cex.lab = 1.1, cex.axis = 1.1,...)
      {
        if (trim) {
          trim <- trim * length(x)
          x <- x[-c(1:trim, (length(x) - trim + 1):length(x))]
        }
        
        hist(x, breaks = breaks, freq = freq, include.lowest = include.lowest,
             right = right, density = density, angle = angle,
             border = "darkgrey", main = main, xlab = xlab, ylab = ylab,
             axes = axes, plot = plot, labels = labels, col = "midnightblue", cex.lab = 1.1, cex.axis = 1.1,
             ...)
      }
    
    the.method <- method
    okones <- boot.par.out$ierstuff == 0
    numsim <- nrow(boot.par.out$theta.hat.star)
    lower.quantile <- (1 - conf.level)/2
    upper.quantile <- 1 - (1 - conf.level)/2
    lelements <- floor(((1 - conf.level)/2) * (numsim + 1))
    uelements <- numsim - lelements
    mlest.out <- boot.par.out$mlest.out
    vcv.matrix <- mlest.out$vcv.matrix
    distribution <- mlest.out$distribution
    theta.hat.star <- boot.par.out$theta.hat.star[okones, ]
    vcv.matrix.star <- boot.par.out$vcv[okones, ]
    theta.hat <- mlest.out$theta.hat
    switch(inference.on, probability = {
      the.time0 <- which
      sigma <- theta.hat[2]
      if (is.logdist(distribution)) the.tran.time0 <- logb(the.time0) else the.tran.time0 <- the.time0
      if (is.null(method)) {
        method <- "boott.logit"
        the.method <- "bootstrap t logistic transformation"
      }
      z0 <- (the.tran.time0 - theta.hat[1])/theta.hat[2]
      theta.hat <- wqmf.phibf(z0, distribution)
      stderror <- (abs(wqmf.phis(z0, distribution))/sigma) *
        sqrt(vcv.matrix[1, 1] + 2 * z0 * vcv.matrix[1, 2] +
               (z0)^2 * vcv.matrix[2, 2])
      z0star <- (the.tran.time0 - theta.hat.star[, 1])/theta.hat.star[,
                                                                      2]
      sigma.star <- theta.hat.star[, 2]
      theta.hat.star <- wqmf.phibf(z0star, distribution)
      stderror.star <- (abs(wqmf.phis(z0star, distribution))/sigma.star) *
        sqrt(vcv.matrix.star[, 1] + 2 * z0star * vcv.matrix.star[,
                                                                 2] + (z0star)^2 * vcv.matrix.star[, 3])
      if (is.null(parameter.name)) parameter.name <- paste("widehat(F)(",
                                                           format(the.time0), ")", sep = "")
      okones <- stderror.star > 0
      theta.hat.star <- theta.hat.star[okones]
      stderror.star <- stderror.star[okones]
    }, quantile = {
      quant.now <- quant(which, distribution)
      theta.hat <- theta.hat[1] + quant.now * theta.hat[2]
      names(theta.hat) <- ""
      stderror <- sqrt(vcv.matrix[1, 1] + 2 * quant.now * vcv.matrix[1,
                                                                     2] + (quant.now)^2 * vcv.matrix[2, 2])
      theta.hat.star <- theta.hat.star[, 1] + quant.now * theta.hat.star[,
                                                                         2]
      stderror.star <- sqrt(vcv.matrix.star[, 1] + 2 * quant.now *
                              vcv.matrix.star[, 2] + (quant.now)^2 * vcv.matrix.star[,
                                                                                     3])
      if (is.logdist(distribution)) {
        theta.hat <- exp(theta.hat)
        stderror <- stderror * theta.hat
        theta.hat.star <- exp(theta.hat.star)
        stderror.star <- stderror.star * theta.hat.star
        if (is.null(method)) {
          method <- "boott.log"
          the.method <- "bootstrap t log transformation"
        }
      } else {
        if (is.null(method)) {
          method <- "boott.notran"
          the.method <- "bootstrap t no transformation"
        }
      }
      if (is.null(parameter.name)) parameter.name <- paste("widehat(t)[",
                                                           format(which, digits = 3),"]", sep = "")
    }, parameter = {
      if (which < 1 || which > length(theta.hat)) stop(paste("Parameter",
                                                             which, "is out of range.\nLength of theta.hat is",
                                                             length(theta.hat)))
      if (is.null(method)) switch(as.character(mlest.out$kodet[which]),
                                  `1` = {
                                    method <- "boott.notran"
                                    the.method <- "bootstrap t no transformation"
                                  }, `2` = {
                                    method <- "boott.log"
                                    the.method <- "bootstrap t log transformation"
                                  }, `3` = {
                                    method <- "boott.logit"
                                    the.method <- "bootstrap t logistic transformation"
                                  })
      theta.hat.star <- boot.par.out$theta.hat.star[okones,
                                                    which]
      theta.hat <- mlest.out$theta.hat[which]
      stderror <- sqrt(diag(vcv.matrix))[which]
      vcvloc <- floor((which * (which + 1))/2 + 0.1)
      stderror.star <- sqrt(boot.par.out$vcv[okones, vcvloc])
      if (is.null(parameter.name)) parameter.name <- paste("widehat(",names(mlest.out$theta.hat)[which],
                                                           ")", sep = "")
    }, {
      stop(paste(inference.on, "is not a recognized inference.on input"))
    })
    switch(method, boott.notran = , boott = {
      empirical.dist <- sort((theta.hat.star - theta.hat)/stderror.star)
      lower.perc <- empirical.dist[lelements]
      upper.perc <- empirical.dist[uelements]
      lower.bound <- theta.hat - upper.perc * stderror
      upper.bound <- theta.hat - lower.perc * stderror
      local.title <- "Bootstrap-t  Untransformed"
      my.xlab <- paste("Z[", parameter.name, "[boot]]", sep = "")
    }, boott.log = {
      fact <- 1/(theta.hat.star)
      empirical.dist <- sort((logb(theta.hat.star) - logb(theta.hat))/(fact *
                                                                         stderror.star))
      lower.perc <- empirical.dist[lelements]
      upper.perc <- empirical.dist[uelements]
      stderrorlog <- stderror/theta.hat
      lower.bound <- exp(logb(theta.hat) - upper.perc * stderrorlog)
      upper.bound <- exp(logb(theta.hat) - lower.perc * stderrorlog)
      local.title <- "Bootstrap-t  log-transform"
      my.xlab <- paste("Z[log(", parameter.name, "[boot])]", sep = "")
    }, boott.logit = {
      fact <- 1/(theta.hat.star * (1 - theta.hat.star))
      empirical.dist <- sort((qlogis(theta.hat.star) - qlogis(theta.hat))/(fact *
                                                                             stderror.star))
      lower.perc <- empirical.dist[lelements]
      upper.perc <- empirical.dist[uelements]
      w.lower <- exp((upper.perc * stderror)/(theta.hat * (1 -
                                                             theta.hat)))
      lower.bound <- theta.hat/(theta.hat + (1 - theta.hat) *
                                  w.lower)
      w.upper <- exp((lower.perc * stderror)/(theta.hat * (1 -
                                                             theta.hat)))
      upper.bound <- theta.hat/(theta.hat + (1 - theta.hat) *
                                  w.upper)
      local.title <- "Bootstrap-t logit-transformed"
      my.xlab <- paste("Z[logit(", parameter.name, "[boot])]", sep = "")
    }, percentile = {
      empirical.dist <- sort(theta.hat.star)
      lower.perc <- empirical.dist[lelements]
      upper.perc <- empirical.dist[uelements]
      lower.bound <- lower.perc
      upper.bound <- upper.perc
      local.title <- "Bootstrap Estimates"
      my.xlab <- paste(parameter.name, "[boot]", sep = "")
    }, {
      stop(paste(method, "bootstrap method not recognized"))
    })
    big.ones <- empirical.dist > 1e+35
    if (any(big.ones)) {
      empirical.dist <- empirical.dist[!big.ones]
      warning(paste("Empirical bootstrap distribution values were extremely large using method",
                    method, "\nThese were filtered out"))
    }
    wqm.hist(empirical.dist, nclass = 20, xlab = "", ylab = "",
             main = "", yaxt = "n", col = "midnightblue", cex.axis = 1.1, cex.lab = 1.1)
    if (is.null(my.title))
      my.title <- paste(get.data.title(boot.par.out$mlest.out$data.ld),
                        boot.par.out$mlest.out$distribution, "Distribution\n",
                        local.title)
    else my.title <- paste(local.title)
    title(main = my.title, cex = cex.title)
    title(xlab = parse(text = my.xlab), cex.lab = cex.labs)
    if (!is.null(cum.method) && method == cum.method) {
      plot.boot.cdf(empirical.dist, my.xlab, my.title, trim = 0.005,
                    lower.quantile, lower.perc, upper.quantile, upper.perc)
    }
    if (inference.on == "parameter" && generic.distribution(distribution) ==
        "weibull" && which == 2) {
      parameter.name <- "beta"
      tmp <- 1/lower.bound
      lower.bound <- 1/upper.bound
      upper.bound <- tmp
    }
    cat("\n\nUsing the", the.method, "method,\nan approximate",
        floor(100 * conf.level + 0.01), "percent confidence interval \nfor",
        parameter.name, "is", paste("[", format(lower.bound),
                                    ",    ", format(upper.bound), "]", sep = ""), ".\n")
    invisible(list(method = method, conf.level = conf.level,
                   lower.perc = lower.perc, upper.perc = upper.perc, lower.bound = lower.bound,
                   upper.bound = upper.bound))
  }

#'
#'

plot.boot.cdf <-
  function (x, my.xlab, my.title, trim, lower.quantile, lower.perc,
            upper.quantile, upper.perc, ylabs = c("0", ".5", "1"),...)
  {
    old.par <- par(err = -1, mar = c(4.5, 4.1, 3.5, 2) + 0.1)
    on.exit(par(old.par))
    lenx <- length(x)
    if (trim) {
      trim <- trim * length(x)
      x <- x[-c(1:trim, (lenx - trim + 1):lenx)]
    }
    lenx <- length(x)
    rangex <- range(x)
    plot(rangex, c(0, 1), xlab = "", ylab = "", type = "n", yaxt = "n", cex.axis = 1.1, cex.lab = 1.1)
    axis(2, at = as.numeric(ylabs), labels = ylabs, cex.axis = 1.1, cex.lab = 1.1)
    lines(x, ((1:lenx) - 0.5)/lenx, lwd = 2, col = "midnightblue")
    lines(c(lower.perc[1], lower.perc[1]), c(-1, lower.quantile[1]),
          lty = 3, lwd = 2)
    lines(c(-100, lower.perc[1]), c(lower.quantile[1], lower.quantile[1]),
          lty = 3, lwd = 2)
    lines(c(upper.perc[1], upper.perc[1]), c(-1, upper.quantile[1]),
          lty = 3, lwd = 2)
    lines(c(-100, upper.perc[1]), c(upper.quantile[1], upper.quantile[1]),
          lty = 3, lwd = 2)
    title(main = my.title)
    title(xlab = parse(text = my.xlab), ylab = "Bootstrap cdf", cex.lab = 1.1)
  }

#'
#'

plot.boot.npar.npar.out <-
  function (x, xlog = F, xlab = get.time.units(data.ld),
            xlim = c(NA, NA), ylim = c(NA, NA), original.par = F,
            my.title = NULL, number.boot.plot = 50,...)
  {
    old.par <- par(mar = c(4.5, 5, 3.5, 2) + 0.1, err = -1)
    if (original.par)
      on.exit(par(old.par))
    cdfest.out <- x$cdfest.out
    f.hat.star <- x$f.hat.star
    number.boot.plot <- min(nrow(f.hat.star), number.boot.plot)
    data.ld <- cdfest.out$data.ld
    cdpoints.out <- cdpoints(cdfest.out)
    xrna <- is.na(xlim)
    if (any(xrna))
      xlim[xrna] <- range(cdpoints.out$yplot)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
      ylim[yrna] <- range(cdpoints.out$pplot)[yrna]
    if (is.null(my.title))
      my.title <- paste(get.data.title(data.ld), "\n", "Nonparametric CDF Estimate")
    if (xlog)
      logger <- "x"
    else logger <- ""
    cdfest.out$p[cdfest.out$p <= 0] <- 0
    plot(xlim, ylim, log = logger, type = "n", xlab = "",
         ylab = "", las = 1, cex.axis = 1.1)
    at.point <- cdfest.out$p == cdfest.out$q
    over.interval <- !(cdfest.out$p == cdfest.out$q)
    if (any(at.point))
      points.default(cdfest.out$p[at.point], cdfest.out$prob[at.point],
                     pch = 16, cex = (2 * GetSMRDDefault("SMRD.point.size"))/100)
    if (any(over.interval))
      segments(cdfest.out$p[over.interval], cdfest.out$prob[over.interval],
               cdfest.out$q[over.interval], cdfest.out$prob[over.interval],
               lwd = 6)
    for (i in 1:number.boot.plot) {
      if (any(at.point))
        points.default(cdfest.out$p[at.point], f.hat.star[i,
                                                          at.point], cex = (1 * GetSMRDDefault("SMRD.point.size"))/100)
      if (any(over.interval))
        segments(cdfest.out$p[over.interval], f.hat.star[i,
                                                         over.interval], cdfest.out$q[over.interval],
                 f.hat.star[i, over.interval], lty = 3, col = "darkblue")
    }
    title.line = 0.5
    #mtext(side = 3, line = title.line, outer = F, text = my.title, cex = 1.5)
    title(xlab = xlab, cex.lab = 1.1)
    title(ylab = "Proportion Failing", cex.lab = 1.1, mgp = c(4,
                                                              1, 0))
    invisible(cdfest.out)
  }

#'
#'

plot.boot.npar.par.out <-
  function (x, xlim = c(NA, NA), ylim = c(NA,
                                                NA), my.title = NULL, grids = F, title.option = GetSMRDDefault("SMRD.TitleOption"), number.lines.plot = 50,
            xlab = get.time.units(mlest.out$data.ld), number.time.points = 50,
            cex = 1, cex.labs = 1.1, cex.title = 1, parameter.sims = 500, simulate.parameters = F,...)
  {
    
    if (!simulate.parameters) {
      
      mlest.out <- x$mlest.out
      theta.hat <- mlest.out$theta.hat
      distribution <- mlest.out$distribution
      if (is.null(my.title)) {
        my.title <- paste(distribution, "Distribution ML Estimate and Bootstrap Results for\n",
                          get.data.title(mlest.out$data.ld))
      }
      number.lines.plot <- min(nrow(x$theta.hat.star),
                               number.lines.plot)
      xrna <- is.na(xlim)
      if (any(xrna))
        xlim[xrna] <- range(Response(mlest.out$data.ld))[xrna]
      if (is.logdist(distribution)) {
        time <- logseq(xlim[1], xlim[2], length = number.time.points)
        ltime <- logb(time)
      } else {
        ltime <- seq(xlim[1], xlim[2], length = number.time.points)
        time <- ltime
      }
      if (generic.distribution(distribution) == "exponential") {
        sigma <- 1
        z <- (time/mlest.out$theta.hat[1])
      } else {
        sigma <- theta.hat[2]
        z <- (ltime - mlest.out$theta.hat[1])/sigma
      }
      dist.probs <- wqmf.phibf(z, distribution)
      zgood <- dist.probs > 0 & dist.probs < 1
      dist.probs <- dist.probs[zgood]
      old.dist.quantiles <- quant(dist.probs, distribution)
      yrna <- is.na(ylim)
      if (any(yrna))
        ylim[yrna] <- range(dist.probs)[yrna]
      log.of.data <- is.logdist(distribution)
      probpaper(distribution = distribution, xlim = xlim,
                ylim = ylim, my.title = my.title, xlab = xlab,
                grids = F, title.option = title.option, cex = cex, cex.labs = cex.labs,
                cex.title = cex.title)
      dist.quantiles <- quant(dist.probs, distribution)
      old.time <- time
      ntime <- length(time)
      time <- time[-c(1:2, (ntime - 1):ntime)]
      ltime <- ltime[-c(1:2, (ntime - 1):ntime)]
      for (i in 1:number.lines.plot) {
        if (generic.distribution(distribution) == "exponential") {
          sigma <- 1
          z <- (time/x$theta.hat.star[i, 1])
        } else {
          sigma <- x$theta.hat.star[i, 2]
          z <- (ltime - x$theta.hat.star[i,
                                         1])/sigma
        }
        dist.probs <- wqmf.phibf(z, distribution)
        zgood <- dist.probs > 0 & dist.probs < 1
        lines(pp.data(time[zgood], log.of.data), quant(dist.probs[zgood],
                                                       distribution), lwd = 1, col = "darkblue")
      }
      lines(pp.data(old.time, log.of.data), old.dist.quantiles,
            lwd = 5, col = 1)
      invisible()
      
    } else {
      
      mins <- function (x) { order(x)[1] }
      maxs <- function (x) { order(x)[length(x)] }
      mlest.out <- x$mlest.out
      theta.hat <- mlest.out$theta.hat
      param.names <- names(theta.hat)
      distribution <- mlest.out$distribution
      parameter.sims <- min(nrow(x$theta.hat.star),
                            parameter.sims)
      mu <- mlest.out$theta.hat[1]
      if (is.null(my.title)) {
        my.title <- paste(distribution, "Distribution ML Estimate and Bootstrap Results for\n",
                          get.data.title(mlest.out$data.ld))
      }
      if (generic.distribution(distribution) == "exponential") {
        sigma <- 1
      } else {
        sigma <- mlest.out$theta.hat[2]
      }
      mu.hat <- x$theta.hat.star[, 1]
      if (generic.distribution(distribution) == "exponential") {
        sigma.hat <- rep(1, length(mu.hat))
      } else {
        sigma.hat <- x$theta.hat.star[, 2]
      }
      xrna <- is.na(xlim)
      if (any(xrna))
        xlim[xrna] <- range(mu.hat)[xrna]
      yrna <- is.na(ylim)
      if (any(yrna))
        ylim[yrna] <- range(sigma.hat)[yrna]
      plot.paper(xlim, ylim, my.title = my.title, grids = F, cex.title = 0.8)
      title(xlab = parse(text = param.names[1]), 
            ylab = parse(text = param.names[2]),cex.lab = 1.5)
      corners <- unique(c(mins(mu.hat), mins(sigma.hat), maxs(mu.hat),
                          maxs(sigma.hat)))
      the.ones <- c(sample((1:length(sigma.hat))[-corners], parameter.sims -
                             length(corners)), corners)
      points.default(mu, sigma, pch = 16, cex = (2.5 * GetSMRDDefault("SMRD.point.size"))/100, 
                     col = 1)
      points.default(mu.hat[the.ones], sigma.hat[the.ones], pch = 16,
                     cex = (0.5 * GetSMRDDefault("SMRD.point.size"))/100,
                     col = "darkblue")
      invisible()
    }
    
    
  }

#'
#'

summary.boot.npar.npar.out <-
  function (object, time.index = last.one, method = "boott.logit",
            my.title = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            ccorrect = T, cum.method = NULL,...)
  {
    is.min <- function (x) { x == min(x) }
    is.maxele <- function (x) { seq(1:length(x))[is.min(.Uminus(x))][1] }
    
    wqm.hist <-
      function (x, breaks = "Sturges", freq = NULL, include.lowest = TRUE,
                right = TRUE, density = NULL, angle = 45, col = NULL, border = NULL,
                main = "", xlim = NULL, ylim = NULL, xlab = deparse(substitute(x)),
                ylab = "Frequency", axes = TRUE, plot = TRUE, labels = FALSE,
                nclass = NULL, trim = F, ...)
      {
        if (trim) {
          trim <- trim * length(x)
          x <- x[-c(1:trim, (length(x) - trim + 1):length(x))]
        }
        
        hist(x, breaks = breaks, freq = freq, include.lowest = include.lowest,
             right = right, density = density, angle = angle,
             border = border, main = main, xlab = xlab, ylab = ylab,
             axes = axes, plot = plot, labels = labels, col = "midnightblue",
             ...)
      }
    
    
    numsim <- nrow(object$f.hat.star)
    lower.quantile <- (1 - conf.level)/2
    upper.quantile <- 1 - (1 - conf.level)/2
    lelements <- floor(((1 - conf.level)/2) * (numsim + 1))
    uelements <- numsim - lelements
    last.one <- length(object$p)
    okones <- object$f.hat.star[, time.index] > 0 &
      object$f.hat.star[, time.index] < 1
    which.zeros <- object$f.hat.star[, time.index] <=
      0
    number.zeros <- length(object$f.hat.star[which.zeros,
                                             time.index])
    print(paste("Proportion of 0's = ", number.zeros/numsim))
    which.ones <- object$f.hat.star[, time.index] >=
      1
    number.ones <- length(object$f.hat.star[which.ones,
                                            time.index])
    print(paste("Proportion of 1's = ", number.ones/numsim))
    number.extreme.title <- ""
    if (number.zeros > 0)
      number.extreme.title <- paste(number.extreme.title, "Proportion at -infinity = ",
                                    format(number.zeros/numsim))
    if (number.ones > 0)
      number.extreme.title <- paste(number.extreme.title, "Proportion at infinity = ",
                                    format(number.ones/numsim))
    if (ccorrect && number.zeros != numsim && number.ones !=
        numsim) {
      stderror.star <- object$stderror.star[, time.index]
      f.hat.star <- object$f.hat.star[, time.index]
      min.index <- is.maxele(-f.hat.star[!which.zeros])
      min01 <- f.hat.star[!which.zeros][min.index]/2
      sdmin01 <- stderror.star[!which.zeros][min.index]/2
      stderror.star[which.zeros] <- sdmin01
      f.hat.star[which.zeros] <- min01
      max.index <- is.maxele(f.hat.star[!which.ones])
      max01 <- 1 - (1 - f.hat.star[!which.ones][max.index])/2
      sdmax01 <- stderror.star[!which.ones][max.index]/2
      stderror.star[which.ones] <- sdmax01
      f.hat.star[which.ones] <- max01
    } else {
      f.hat.star <- object$f.hat.star[okones, time.index]
      stderror.star <- object$stderror.star[okones,
                                            time.index]
    }
    f.hat <- object$f.hat[time.index]
    if (f.hat <= 0) {
      warning("f.hat=0")
      return(0)
    }
    stderror <- object$sd[time.index]
    switch(method, boott = {
      dist <- sort((f.hat.star - f.hat)/stderror.star)
      if (ccorrect) {
        lower.perc <- dist[lelements]
        upper.perc <- dist[uelements]
      } else {
        lower.perc <- c(rep(-1e+20, number.zeros), dist,
                        rep(1e+20, number.ones))[lelements]
        upper.perc <- c(rep(-1e+20, number.zeros), dist,
                        rep(1e+20, number.ones))[uelements]
      }
      lower.bound <- f.hat - upper.perc * stderror
      upper.bound <- f.hat - lower.perc * stderror
      local.title <- "Bootstrap-t Untransformed"
      my.xlab <- "Z[widehat(F)[boot]]"
    }, boott.logit = {
      fact <- 1/(f.hat.star * (1 - f.hat.star))
      dist <- sort((qlogis(f.hat.star) - qlogis(f.hat))/(fact *
                                                           stderror.star))
      if (ccorrect) {
        lower.perc <- dist[lelements]
        upper.perc <- dist[uelements]
      } else {
        lower.perc <- c(rep(-1e+20, number.zeros), dist,
                        rep(1e+20, number.ones))[lelements]
        upper.perc <- c(rep(-1e+20, number.zeros), dist,
                        rep(1e+20, number.ones))[uelements]
      }
      w.upper <- exp((upper.perc * stderror)/(f.hat * (1 -
                                                         f.hat)))
      w.lower <- exp((lower.perc * stderror)/(f.hat * (1 -
                                                         f.hat)))
      lower.bound <- f.hat/(f.hat + (1 - f.hat) * w.lower)
      upper.bound <- f.hat/(f.hat + (1 - f.hat) * w.upper)
      local.title <- "Bootstrap-t logit-transformed"
      my.xlab <- "Z[logit~widehat(F)[boot]]"
    }, boott.asin = {
      fact <- 1/(2 * sqrt(f.hat.star * (1 - f.hat.star)))
      dist <- sort((asin(sqrt(f.hat.star)) - asin(sqrt(f.hat)))/(fact *
                                                                   stderror.star))
      if (ccorrect) {
        lower.perc <- dist[lelements]
        upper.perc <- dist[uelements]
      } else {
        lower.perc <- c(rep(0, number.zeros), dist, rep(pi/2,
                                                        number.ones))[lelements]
        upper.perc <- c(rep(0, number.zeros), dist, rep(pi/2,
                                                        number.ones))[uelements]
      }
      lower.bound <- -1
      upper.bound <- 2
      local.title <- "Bootstrap-t arcsin-sqrt-transformed"
      my.xlab <- "Z[arcsin~sqrt(widehat(F))[boot]]"
    }, percentile = {
      dist <- sort(f.hat.star)
      if (ccorrect) {
        lower.perc <- dist[lelements]
        upper.perc <- dist[uelements]
      } else {
        lower.perc <- c(rep(0, number.zeros), dist, rep(1,
                                                        number.ones))[lelements]
        upper.perc <- c(rep(0, number.zeros), dist, rep(1,
                                                        number.ones))[uelements]
      }
      lower.bound <- lower.perc
      upper.bound <- upper.perc
      local.title <- "Bootstrap Estimates"
      my.xlab <- "widehat(F)[boot]"
    }, {
      stop("method not recognized")
    })
    wqm.hist(dist, nclass = 20, trim = 0.005, xlab = "", yaxt = "n",
             col = 4)
    if (is.null(my.title))
      my.title <- paste(local.title, "\n", number.extreme.title)
    title(main = my.title)
    title(xlab = my.xlab, cex.lab = 1.1)
    if (!is.null(cum.method) && method == cum.method) {
      plot.boot.cdf(dist, my.xlab, my.title, trim = 0.005,
                    lower.quantile, lower.perc, upper.quantile, upper.perc)
    }
    return(list(method = method, conf.level = conf.level, lower.perc = lower.perc,
                upper.perc = upper.perc, lower.bound = lower.bound, upper.bound = upper.bound))
  }

#'
#'

summary.boot.npar.par.out <-
  function (object, inference.on = "parameter", which = 1,
            method = NULL, parameter.name = NULL, my.title = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            cum.method = NULL, do.compare = F,...)
  {
    if (do.compare)
      return.stuff <- compare.summary.boot.npar.par.out(boot.par.out = object,
                                                        inference.on = inference.on, which = which, parameter.name = parameter.name,
                                                        my.title = my.title, conf.level = conf.level, cum.method = cum.method)
    else return.stuff <- focus.boot.npar.par.out(boot.par.out = object,
                                                 inference.on = inference.on, which = which, method = method,
                                                 parameter.name = parameter.name, my.title = my.title,
                                                 conf.level = conf.level, cum.method = cum.method)
    invisible(return.stuff)
  }

#'
#'

which.boot.method.to.use <-
  function (inference.on, which, mlest.out)
  {
    switch(inference.on, probability = {
      method <- "boott.logit"
    }, quantile = {
      if (is.logdist(mlest.out$distribution)) method <- "boott.log" else method <- "boott.notran"
    }, parameter = {
      if (which < 1 || which > length(mlest.out$theta.hat)) stop(paste("Parameter",
                                                                       which, "is out of range.\nLength of theta.hat is",
                                                                       length(mlest.out$theta.hat)))
      switch(as.character(mlest.out$kodet[which]), `1` = {
        method <- "boott.notran"
      }, `2` = {
        method <- "boott.log"
      }, `3` = {
        method <- "boott.logit"
      })
    })
    return(method)
  }

#'
#'

boot.filter <-
  function (boot.results) 
  {
    ierstuff <- boot.results$ierstuff
    start.length <- length(ierstuff)
    theta.hat.star <- as.matrix(boot.results$theta.hat.star)
    the.uniques <- unique(apply(theta.hat.star, 1, paste, sep = "", 
                                collapse = ","))
    cat("\n There were ", length(the.uniques), "unique bootstrap samples out of\n", 
        start.length, "total bootstrap samples\n")
    boot.results$theta.hat.star <- theta.hat.star[ierstuff == 
                                                    0, ]
    boot.results$vcv <- boot.results$vcv[ierstuff == 0, ]
    boot.results$likelihood <- boot.results$likelihood[ierstuff == 
                                                         0]
    boot.results$ierstuff <- boot.results$ierstuff[ierstuff == 
                                                     0]
    end.length <- length(boot.results$ierstuff)
    if (start.length > end.length) {
      the.percent <- 100 * ((start.length - end.length)/start.length)
      cat("\n A total of", start.length - end.length, "of", 
          start.length, paste("(", the.percent, "%)", sep = ""), 
          "bad bootstrap samples\n were removed because of convergence difficulties.\n These were probably samples with 0 failures \n or no variability in the response.\n\n")
    }
    invisible(boot.results)
  }
