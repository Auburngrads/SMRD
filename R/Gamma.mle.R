Gamma.mle <-
  function (data.ld,debug1= F, theta.start = NULL)
  {
    options(digits = 5)
    f.origparam <- function(thetatran, model) {
      tp1 <- exp(thetatran[1])
      kappa <- exp(thetatran[2])
      pmiddle <- (model$p2 + model$p1)/2
      theta <- tp1/qgamma(pmiddle, shape = kappa)
      thetaorig <- c(theta, kappa)
      names(thetaorig) <- model$orig.param.names
      return(thetaorig)
    }
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    probs <- cdfest(data.ld)$prob
    p1 <- min(probs[probs > 0])/2
    p2 <- 0.9 * max(probs)
    orig.param.names <- c("theta", "kappa")
    t.param.names <- c("logtp1", "logkappa")
    model <- list(p1 = p1, p2 = p2, distribution = "gamma", t.param.names = t.param.names,
                  orig.param.names = orig.param.names)
    if (is.null(theta.start)) {
      ls.gmle.out <- ls.mle(data.ld, distribution = "Lognormal")
      mu <- ls.gmle.out$origparam["location"]
      sigma <- ls.gmle.out$origparam["scale"]
      pmiddle <- (model$p2 + model$p1)/2
      logtp1 <- qnorm(pmiddle, mu, sigma)
      theta.start <- c(logtp1, 1)
    }
    gmle.out <- gmle(log.like = general.dist.log.like, data.ld = data.ld,
                     theta.start = theta.start, model = model, f.origparam = f.origparam,
                     t.param.names = t.param.names, orig.param.names = orig.param.names,
                    debug1= debug1)
    return(gmle.out)
  }
