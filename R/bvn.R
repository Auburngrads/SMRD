bvn <-
function (mu1, 
          mu2, 
          sd1, 
          sd2, 
          rho, 
          ah, 
          ak,
          kprint = 0) 
{
    number <- max(length(ah), 
                  length(ak), 
                  length(sd1), 
                  length(sd2), 
                  length(mu1), 
                  length(mu2), 
                  length(rho))
    
    ah  <- expand.vec(ah, number)
    ak  <- expand.vec(ak, number)
    mu1 <- expand.vec(mu1, number)
    mu2 <- expand.vec(mu2, number)
    v1  <- expand.vec(sd1 * sd1, number)
    v2  <- expand.vec(sd2 * sd2, number)
    c12 <- expand.vec(rho * sd1 * sd2, number)
    
    the.frame <- data.frame(ah = ah, 
                            ak = ak, 
                            mu1 = mu1, 
                            mu2 = mu2,
                            v1 = v1, 
                            v2 = v2, 
                            c12 = c12)
    
    zout <- SBVN(as.double(the.frame$ah), 
                 as.double(the.frame$ak), 
                 as.double(the.frame$mu1), 
                 as.double(the.frame$mu2), 
                 as.double(the.frame$v1),
                 as.double(the.frame$v2), 
                 as.double(the.frame$c12), 
                 double(number),
                 as.integer(number),
                 as.integer(kprint))
    
    return(zout$prob)
    
}

#'
#'

bvn.fail.contrib.delta <-
  function (yresp, theta, eps = 1.00000000000001e-05) 
  {
    lower <- bvn(mu1 = theta[, 1], mu2 = theta[, 2], sd1 = theta[, 
                                                                 3], sd2 = theta[, 4], rho = theta[, 5], ah = yresp[, 
                                                                                                                    1] - eps, ak = yresp[, 2] - eps)
    upper <- bvn(mu1 = theta[, 1], mu2 = theta[, 2], sd1 = theta[, 
                                                                 3], sd2 = theta[, 4], rho = theta[, 5], ah = yresp[, 
                                                                                                                    1] + eps, ak = yresp[, 2] - eps)
    strip.long <- lower - upper
    lower <- bvn(mu1 = theta[, 1], mu2 = theta[, 2], sd1 = theta[, 
                                                                 3], sd2 = theta[, 4], rho = theta[, 5], ah = yresp[, 
                                                                                                                    1] - eps, ak = yresp[, 2] + eps)
    upper <- bvn(mu1 = theta[, 1], mu2 = theta[, 2], sd1 = theta[, 
                                                                 3], sd2 = theta[, 4], rho = theta[, 5], ah = yresp[, 
                                                                                                                    1] + eps, ak = yresp[, 2] + eps)
    strip.short <- lower - upper
    cat("bvn.fail.contrib.delta eps prob1,prob2=", eps, format(strip.long[1]), 
        format(lower[1], upper[1], strip.short[1]), format(strip.long[1] - 
                                                             strip.short[1]), "\n")
    diff <- strip.long - strip.short
    diff[diff <= 0] <- 1e-15
    return(log(diff))
  }


#'
#'

bvn.fail.contrib.density <-
  function (yresp, theta, eps = 1.00000000000001e-05) 
  {
    logprob <- dbvnl(yresp[, 1], yresp[, 2], mux = theta[, 1], 
                     muy = theta[, 2], sdx = theta[, 3], sdy = theta[, 4], 
                     rho = theta[, 5]) + log(4 * eps^2)
    return(logprob)
  }

#'
#'

bvn.fail.contrib <-
  function (yresp, failure.mode, theta, eps = 0.005, like.method = NULL) 
  {
    if (is.null(like.method)) {
      warning("Likelihood method not specified in bvn.fail.contrib---setting to delta")
      like.method <- "delta"
    }
    the.contribution <- switch(like.method, delta = {
      bvn.fail.contrib.delta(yresp = yresp, theta = theta, 
                             eps = eps)
    }, density = {
      the.contribution <- bvn.fail.contrib.density(yresp = yresp, 
                                                   theta = theta, eps = eps)
    }, {
      stop(paste(like.method, "is not a recognized like.method"))
    })
    return(the.contribution)
  }

#'
#'

bvn.log.like <-
  function (thetain)
  {
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE,"iter.count", iter.count )
    debug1<- get(envir = .frame0,  "debug1")
    data.ld <- get(envir = .frame0,  "data.ld")
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    switch(distribution, bvn = {
      yresp <- cbind(Response(data.ld), xmat(data.ld)[, 1])
    }, bvln = {
      yresp <- logb(cbind(Response(data.ld), xmat(data.ld)[,
                                                           1]))
    }, {
      stop(cat(distribution, "is unrecognized distribution\n"))
    })
    the.censor.codes <- censor.codes(data.ld)
    like.method <- model$like.method
    f.origparam <- model$f.origparam
    theta.origparam <- f.origparam(thetain, model)
    special.stuff <- get(envir = .frame0,  "special.stuff")
    plot.iter <- special.stuff$plot.iter
    case.weights <- case.weights(data.ld)
    if (debug1>= 4 && iter.count < 2)
      browser()
    if (iter.count < 2)
      browser()
    fail.part <- 0
    rcensor.part <- 0
    exact.obs <- the.censor.codes == 1
    ne.censor.obs <- the.censor.codes == 2
    if (any(exact.obs))
      fail.part <- sum(case.weights[exact.obs] * bvn.fail.contrib(yresp = yresp[exact.obs,
                                                                                 , drop = F], theta = theta.origparam, like.method = like.method))
    if (any(ne.censor.obs))
      rcensor.part <- sum(case.weights[ne.censor.obs] * bvn.rcen.contrib.lognormal(yresp = yresp[ne.censor.obs,
                                                                                                  , drop = F], theta = theta.origparam))
    the.log.like <- fail.part + rcensor.part
    if (debug1> 0 && iter.count%%10 == 0)
      cat("bvn.log.like", iter.count, format(c(fail.part, rcensor.part,
                                               the.log.like, theta.origparam)), "\n")
    return(.Uminus((the.log.like)))
  }

#'
#'

bvn.mle <-
  function (data.ld, distribution = "bvn",debug1= F, like.method = "density", 
            theta.start = NULL) 
  {
    switch(distribution, bvn = {
      yresp <- cbind(Response(data.ld), xmat(data.ld)[, 1])
    }, bvln = {
      yresp <- logb(cbind(Response(data.ld), xmat(data.ld)[, 
                                                           1]))
    }, {
      stop(cat(distribution, "is unrecognized distribution\n"))
    })
    number.cases <- nrow(yresp)
    the.case.weights <- case.weights(data.ld)
    options(digits = 5)
    the.censor.codes <- censor.codes(data.ld)
    assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    f.tranparam <- function(thetaorig, model) {
      mu.x <- thetaorig[1]
      mu.y <- thetaorig[2]
      logsigma.x <- log(thetaorig[3])
      logsigma.y <- log(thetaorig[4])
      logitrho <- log(thetaorig[5]/(1 - thetaorig[5]))
      thetatran <- c(mu.x, mu.y, logsigma.x, logsigma.y, logitrho)
      names(thetatran) <- model$t.param.names
      return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
      mu.x <- thetatran[1]
      mu.y <- thetatran[2]
      sigma.x <- exp(thetatran[3])
      sigma.y <- exp(thetatran[4])
      rho <- exp(thetatran[5])/(1 + exp(thetatran[5]))
      thetaorig <- c(mu.x, mu.y, sigma.x, sigma.y, rho)
      names(thetaorig) <- model$orig.param.names
      return(thetaorig)
    }
    orig.param.names <- c("mu.x", "mu.y", "sigma.x", "sigma.y", 
                          "rho")
    t.param.names <- c("mu.x", "mu.y", "logsigma.x", "logsigma.y", 
                       "logit.rho")
    model <- list(distribution = distribution, like.method = like.method, 
                  orig.param.names = orig.param.names, t.param.names = t.param.names)
    if (is.null(theta.start)) {
      mu.x <- mean(yresp[, 1])
      mu.y <- mean(yresp[, 2])
      sigma.x <- sqrt(var(yresp[, 1]))
      sigma.y <- sqrt(var(yresp[, 2]))
      rho <- cor(yresp[, 1], yresp[, 2])
      theta.start <- c(mu.x, mu.y, sigma.x, sigma.y, rho)
      names(theta.start) <- model$orig.param.names
      print(theta.start)
    }
    gmle.out <- gmle(data.ld = data.ld, log.like = bvn.log.like, 
                     theta.start = theta.start, model = model, f.origparam = f.origparam, 
                     t.param.names = t.param.names, orig.param.names = orig.param.names, 
                     debug1= debug1)
    theta.hat <- gmle.out$est.out$x
    return(gmle.out)
  }

#'
#'

bvn.rcen.contrib.lognormal <-
  function (yresp, theta) 
  {
    log(bvn(mu1 = theta[, 1], mu2 = theta[, 2], sd1 = theta[, 
                                                            3], sd2 = theta[, 4], rho = theta[, 5], ah = yresp[, 
                                                                                                               1], ak = yresp[, 2]))
  }

#'
#'

bvnRectangle <-
  function (xlow, xhigh, ylow, yhigh, mu1 = 0, mu2 = 0, sd1 = 1, 
            sd2 = 1, rho) 
  {
    lower <- bvn(mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2, 
                 rho = rho, ah = xlow, ak = ylow)
    upper <- bvn(mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2, 
                 rho = rho, ah = xhigh, ak = ylow)
    strip.long <- lower - upper
    lower <- bvn(mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2, 
                 rho = rho, ah = xlow, ak = yhigh)
    upper <- bvn(mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2, 
                 rho = rho, ah = xhigh, ak = yhigh)
    strip.short <- lower - upper
    return((strip.long - strip.short))
  }

#'
#'

bvnsw <-
  function (mu1, mu2, sd1, sd2, rho, ah, ak, numsig = 10) 
  {
    infinity1 <- mu1 + numsig * sd1
    infinity2 <- mu2 + numsig * sd2
    up <- bvn(mu1, mu2, sd1, sd2, rho, ah, -infinity2)
    right <- bvn(mu1, mu2, sd1, sd2, rho, -infinity1, ak)
    ne <- bvn(mu1, mu2, sd1, sd2, rho, ah, ak)
    return(1 - up - right + ne)
  }

